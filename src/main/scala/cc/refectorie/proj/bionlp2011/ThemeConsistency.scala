package cc.refectorie.proj.bionlp2011

import cc.factorie._
import cc.refectorie.proj.factorieie.data._
import java.lang.String
import collection.mutable.{ListBuffer, ArrayBuffer, HashMap}
import java.util.concurrent.ConcurrentHashMap
import collection.JavaConversions
import cc.refectorie.proj.factorieie.util.Util
import java.io.FileOutputStream

trait ThemeModule extends Module {
  type Observation = RelationMention

  def triggerScorer: TriggerScorer

  def argumentScorer: ArgFeatureScorer


  override def loadWeights(prefix: String) {
    val triggerIs = Util.getStreamFromFileOrClassPath(prefix + "/trigger.weights")
    triggerScorer.weights.load(triggerIs)
    val argIs = Util.getStreamFromFileOrClassPath(prefix + "/arg.weights")
    argumentScorer.weights.load(argIs)
  }
  override def storeWeights(prefix: String)  {
    val triggerOut = new FileOutputStream(createDirs(prefix + "/trigger.weights"))
    triggerScorer.weights.store(triggerOut)
    val argOut = new FileOutputStream(createDirs(prefix + "/arg.weights"))
    argumentScorer.weights.store(argOut)

  }
  val doTask2 = Conf.get("doTask2", false)

  override def features(obs: Observation, state: State) = {
    val triggerFeats = triggerScorer.globalFeatures(obs.label, state(obs.label))
    val argFeats = new FeatureVector
    for (arg <- obs.argumentCandidates) {
      argFeats.add(argumentScorer.globalFeatures(arg.role, state(arg.role)), 1.0)
    }
    new GlobalFeatureVector(Map(triggerScorer -> triggerFeats, argumentScorer -> argFeats))
  }

  override def featureDelta(obs: Observation, gold: State, guess: State) = {
    val triggerFeats = new FeatureVector
    if (gold(obs.label) != guess(obs.label)) {
      triggerFeats.add(triggerScorer.globalFeatures(obs.label, gold(obs.label)), 1.0)
      triggerFeats.add(triggerScorer.globalFeatures(obs.label, guess(obs.label)), -1.0)
    }
    val argFeats = new FeatureVector
    for (arg <- obs.argumentCandidates; if (gold(arg.role) != guess(arg.role))) {
      argFeats.add(argumentScorer.globalFeatures(arg.role, gold(arg.role)), 1.0)
      argFeats.add(argumentScorer.globalFeatures(arg.role, guess(arg.role)), -1.0)
    }
    new GlobalFeatureVector(Map(triggerScorer -> triggerFeats, argumentScorer -> argFeats))
  }


  override def weights(obs: Observation) = {
    new GlobalFeatureVector(
      Map(triggerScorer -> triggerScorer.weights, argumentScorer -> argumentScorer.weights))
  }

  override def avgWeights(obs: Observation) = {
    new GlobalFeatureVector(
      Map(triggerScorer -> triggerScorer.averagingWeights, argumentScorer -> argumentScorer.averagingWeights))
  }



  override def weightsUpdated(obs: Observation): Unit = {
    triggerScorer.clearCache
    argumentScorer.clearCache
  }
}

/**
 * Maximizes local scores for edges and triggers, while ensuring that there is at least one  
 * @author sriedel
 */
trait ThemeConsistency extends CanAverageParameters with StandardModule with ThemeModule {

  import BioNLPConstants._

  type Role = RelationMentionArgument#Role

  val triggerScorer = new TriggerScorer {
    def useAverage: Boolean = ThemeConsistency.this.useAverage
  }
  val argumentScorer = new ArgFeatureScorer {
    def useAverage: Boolean = ThemeConsistency.this.useAverage
  }

  def scoreTrigger(label: RelationMention#Label, penalties: Penalties, value: String): Double = {
    penalties.penalty(label, value) + triggerScorer.score(label, value)
  }

  def scoreArgument(label: Role, penalties: Penalties, value: String): Double = {
    penalties.penalty(label, value) + argumentScorer.score(label, value)
  }

  def bestNoneState(relMention: RelationMention, penalties: Penalties): State = {
    val bestNone = new MutableState
    bestNone(relMention.label) = None
    bestNone.score = scoreTrigger(relMention.label, penalties, None)
    for (arg <- relMention.argumentCandidates) {
      val noneScore = scoreArgument(arg.role, penalties, None)
      bestNone(arg.role) = None
      bestNone.score += noneScore
    }
    bestNone
  }

  def bestState(relMention: RelationMention, penalties: Penalties,
                labelFilter: String => Boolean,
                argFilter: RelationMentionArgument => Boolean): State = {
    val bestEvent = new MutableState
    //todo: all the scores can be precomputed in the inferMAP method
    val bestLabel = BioNLPConstants.types.filter(labelFilter(_)).
      maxByDouble(scoreTrigger(relMention.label, penalties, _))
    bestEvent(relMention.label) = bestLabel
    val isRegulation = bestLabel.endsWith("egulation")
    bestEvent.score = scoreTrigger(relMention.label, penalties, bestLabel)
    var (argMax, max) = (relMention.argumentCandidates.head.role, Double.NegativeInfinity)
    var themeCount = 0
    for (arg <- relMention.argumentCandidates) {
      val noneScore = scoreArgument(arg.role, penalties, None)
      if (!argFilter(arg)) {
        bestEvent(arg.role) = None
        bestEvent.score += noneScore
      } else {
        val causeScore = if (isRegulation) scoreArgument(arg.role, penalties, Cause) else Double.NegativeInfinity
        val themeScore = scoreArgument(arg.role, penalties, Theme)
        if (causeScore > Math.max(noneScore, themeScore)) {
          bestEvent(arg.role) = Cause
          bestEvent.score += causeScore
        } else if (themeScore > Math.max(noneScore, causeScore)) {
          bestEvent(arg.role) = Theme
          bestEvent.score += themeScore
          themeCount += 1
        } else {
          bestEvent(arg.role) = None
          bestEvent.score += noneScore
        }
        if (themeScore - Math.max(causeScore, noneScore) > max) {
          max = themeScore - Math.max(causeScore, noneScore)
          argMax = arg.role
        }
      }
    }
    if (themeCount == 0) {
      bestEvent(argMax) = Theme
      bestEvent.score += max
    }
    bestEvent

  }


  def bestRegulationState(relMention: RelationMention, penalties: Penalties): State = {
    bestState(relMention, penalties, _.endsWith("egulation"), m => true)
  }

  def bestNonRegulationState(relMention: RelationMention, penalties: Penalties): State = {
    bestState(relMention, penalties, label => label != None && !label.endsWith("egulation"), _.argIsEntityMention)
  }

  def bestNonNoneState(relMention: RelationMention, penalties: Penalties): State = {
    bestState(relMention, penalties, label => label != None, m => true)
  }


  def updateWeights(obs: RelationMention, gold: State, guess: State, param: TrainingParameters) {
    triggerScorer.batchUpdateWeights(Seq(Example(obs.label, gold.value(obs.label), guess.value(obs.label))), param)
    val argExamples = for (arg <- obs.argumentCandidates) yield {
      Example(arg.role.asInstanceOf[Role], gold.value(arg.role), guess.value(arg.role))
    }
    argumentScorer.batchUpdateWeights(argExamples, param)
    //    println(guess)
    //    println(argumentScorer.weightsString)

    //    triggerScorer.updateWeights(obs.label, gold.value(obs.label), guess.value(obs.label), param)
    //    for (arg <- obs.argumentCandidates) {
    //      argumentScorer.updateWeights(arg.role, gold.value(arg.role), guess.value(arg.role), param)
    //    }
  }

  def inferMAP2(relMention: RelationMention, penalties: Penalties) = {

    //    val bestEvent = bestEventState(relMention, penalties)
    val bestEvent = bestState(relMention, penalties, _ != None, m => true)
    val bestNone = bestNoneState(relMention, penalties)

    //if (bestEvent.score > bestNone.score) bestEvent else bestNone
    if (bestEvent.score >= bestNone.score) bestEvent else bestNone

  }

  def inferMAP(relMention: RelationMention, penalties: Penalties) = {

    val bestRegulation = bestRegulationState(relMention, penalties)
    val bestNonRegulation = bestNonRegulationState(relMention, penalties)
    val bestNone = bestNoneState(relMention, penalties)

    val bestAll = Seq(bestNone, bestRegulation, bestNonRegulation).maxByDouble(_.score)
    bestAll

  }

}

trait CoordinatedThemeConsistency extends CanAverageParameters with StandardModule with ThemeModule {
  self =>

  import BioNLPConstants._

  type Role = RelationMentionArgument#Role

  val triggerScorer = new TriggerScorer {
    def useAverage: Boolean = self.useAverage
  }
  val argumentScorer = new ArgFeatureScorer {
    def useAverage: Boolean = self.useAverage
    override def labelFeatures(label: Any) = {
      if (!doTask2) super.labelFeatures(label)
      else {
        if (Theme != label && Cause != label && Participant != label) Seq("Task2Role" -> 1.0, label -> 1.0)
        else Seq(label -> 1.0)
      }
    }
  }

  def scoreTrigger(label: RelationMention#Label, penalties: Penalties, value: String): Double = {
    penalties.penalty(label, value) + triggerScorer.score(label, value)
  }

  class Cluster {
    val args = new ArrayBuffer[Role]

    def set(state: MutableState, value: String) = {
      for (arg <- args) {
        state(arg) = value
      }
    }

    def isEntityMention: Boolean = args.head.owner.argIsEntityMention
    def entityType = if (isEntityMention) args.head.owner.entityMention.entityType.value else "Event"
  }

  def scoreArgument(label: Role, penalties: Penalties, value: String): Double = {
    penalties.penalty(label, value) + argumentScorer.score(label, value)
  }

  def scoreArgument(cluster: Cluster, penalties: Penalties, value: String): Double = {
    var result = 0.0
    for (label <- cluster.args) {
      result += scoreArgument(label, penalties, value)
    }
    result
  }

  def bestNoneState(relMention: RelationMention, penalties: Penalties): State = {
    val bestNone = new MutableState
    bestNone(relMention.label) = None
    bestNone.score = scoreTrigger(relMention.label, penalties, None)
    for (arg <- relMention.argumentCandidates;
         if (arg.argIsRelationMention || !arg.entityMention.tags(EntityTag))) {
      val noneScore = scoreArgument(arg.role, penalties, None)
      bestNone(arg.role) = None
      bestNone.score += noneScore
    }
    if (doTask2) {
      for (arg <- relMention.argumentCandidates; if (arg.argIsEntityMention)) {
        if (arg.entityMention.tags(EntityTag)) {
          val noneScore = scoreArgument(arg.role, penalties, NoSite)
          bestNone(arg.role) = NoSite
          bestNone.score += noneScore
        }
      }
    }

    bestNone
  }

  val clusterCache = new HashMap[RelationMention, Seq[Cluster]]

  def determineClusters(relMention: RelationMention): Seq[Cluster] = {
    val deps = relMention.sentence.getDependencyStructure("mcclosky")
    val clusters = new ArrayBuffer[Cluster]
    for (arg <- relMention.argumentCandidates; if (!arg.argIsEntityMention || !arg.entityMention.tags(EntityTag))) {
      var found = false
      for (existing <- clusters; if (!found)) {
        for (other <- existing.args; if (!found)) {
          val arg1 = arg.arg
          val arg2 = other.owner.arg
          if (arg.arg.getClass == other.owner.arg.getClass) {
            if (Conf.get("coordinateConj", false)) {

              val edges = deps.edges(arg1.head, arg2.head) ++ deps.edges(other.owner.arg.head, arg.arg.head)
              if (edges.exists(_.label.startsWith("conj"))) {
                found = true
                existing.args += arg.role
              }
            }
            if (Conf.get("coordinateHyphens", false)) {
              if (arg1.isInstanceOf[EntityMention] && arg2.isInstanceOf[EntityMention] &&
                (arg1.begin.prevOption == Some(arg2.end) && arg1.begin.word.startsWith("-") ||
                  arg2.begin.prevOption == Some(arg1.end) && arg2.begin.word.startsWith("-"))) {
                found = true
                existing.args += arg.role
              }
            }
            if (Conf.get("coordinateSlashes", false)) {
              if (arg1.isInstanceOf[EntityMention] && arg2.isInstanceOf[EntityMention] &&
                (arg1.begin.prevOption == Some(arg2.end) && arg1.begin.word.startsWith("/") ||
                  arg2.begin.prevOption == Some(arg1.end) && arg2.begin.word.startsWith("/"))) {
                found = true
                existing.args += arg.role
              }
            }
          }

        }
      }
      if (!found) {
        val cluster = new Cluster
        cluster.args += arg.role
        clusters += cluster
      }
    }
    //println("Clusters\n:%s".format(clusters.map(_.args.mkString(",")).mkString("\n")))
    clusters
  }

  def clusters(relMention: RelationMention): Seq[Cluster] = {
    clusterCache.getOrElseUpdate(relMention, determineClusters(relMention))
  }

  def bestProcess(relMention: RelationMention, penalties: Penalties,
                  events: Iterable[String],
                  argFilter: Cluster => Boolean): State = {
    val bestEvent = new MutableState
    //todo: all the scores can be precomputed in the inferMAP method
    bestEvent(relMention.label) = EventSpecs.Process
    val clusters = this.clusters(relMention)
    bestEvent.score = scoreTrigger(relMention.label, penalties, EventSpecs.Process)
    for (cluster <- clusters) {
      val noneScore = scoreArgument(cluster, penalties, None)
      if (!argFilter(cluster)) {
        cluster.set(bestEvent, None)
        bestEvent.score += noneScore
      } else {
        val participantScore = scoreArgument(cluster, penalties, EventSpecs.Participant)
        if (participantScore > noneScore) {
          cluster.set(bestEvent, EventSpecs.Participant)
          bestEvent.score += participantScore
        } else {
          cluster.set(bestEvent, None)
          bestEvent.score += noneScore
        }
      }
    }
    bestEvent

  }


  def bestState(relMention: RelationMention, penalties: Penalties,
                events: Iterable[String],
                argFilter: Cluster => Boolean,
                siteRoles: Set[String] = Set.empty): State = {
    val bestEvent = new MutableState
    //todo: all the scores can be precomputed in the inferMAP method
    val bestLabel = events.maxByDouble(scoreTrigger(relMention.label, penalties, _))
    bestEvent(relMention.label) = bestLabel
    val clusters = this.clusters(relMention)
    if (clusters.size == 0) return bestEvent
    val isRegulation = EventSpecs.regulations(bestLabel)
    bestEvent.score = scoreTrigger(relMention.label, penalties, bestLabel)
    var (argMax, max) = (clusters.head, Double.NegativeInfinity)
    var themeCount = 0
    for (cluster <- clusters) {
      val noneScore = scoreArgument(cluster, penalties, None)
      if (!argFilter(cluster)) {
        cluster.set(bestEvent, None)
        bestEvent.score += noneScore
      } else {
        val causeScore = if (isRegulation) scoreArgument(cluster, penalties, Cause) else Double.NegativeInfinity
        val themeScore = scoreArgument(cluster, penalties, Theme)
        if (causeScore > Math.max(noneScore, themeScore)) {
          cluster.set(bestEvent, Cause)
          bestEvent.score += causeScore
        } else if (themeScore > Math.max(noneScore, causeScore)) {
          cluster.set(bestEvent, Theme)
          bestEvent.score += themeScore
          themeCount += 1
        } else {
          cluster.set(bestEvent, None)
          bestEvent.score += noneScore
        }
        if (themeScore - Math.max(causeScore, noneScore) > max) {
          max = themeScore - Math.max(causeScore, noneScore)
          argMax = cluster
        }
      }
    }
    if (doTask2) {
      for (arg <- relMention.argumentCandidates; if (arg.argIsEntityMention)) {
        if (arg.entityMention.tags(EntityTag)) {
          val (bestLabel, bestScore) = siteRoles.map(r => r -> scoreArgument(arg.role, penalties, r)).maxByDouble(_._2)
          bestEvent(arg.role) = bestLabel
          bestEvent.score += bestScore
        }
      }
    }
    if (themeCount == 0) {
      argMax.set(bestEvent, Theme)
      bestEvent.score += max
    }
    bestEvent

  }

  def updateWeights(obs: RelationMention, gold: State, guess: State, param: TrainingParameters) {
    triggerScorer.batchUpdateWeights(Seq(Example(obs.label, gold.value(obs.label), guess.value(obs.label))), param)
    val argExamples = for (arg <- obs.argumentCandidates) yield {
      Example(arg.role.asInstanceOf[Role], gold.value(arg.role), guess.value(arg.role))
    }
    argumentScorer.batchUpdateWeights(argExamples, param)
    //    triggerScorer.updateWeights(obs.label, gold.value(obs.label), guess.value(obs.label), param)
    //    for (arg <- obs.argumentCandidates) {
    //      argumentScorer.updateWeights(arg.role, gold.value(arg.role), guess.value(arg.role), param)
    //    }
  }

  def inferMAP(relMention: RelationMention, penalties: Penalties) = {

    //best event(cause/theme) event
    val bestRegulations = EventSpecs.regulationGroups.map {
      group =>
        bestState(relMention, penalties, group.names, m => group.types(m.entityType), group.task2roles)
    }
    val bestNonRegulations = EventSpecs.nonRegulationGroups.map {
      group =>
        bestState(relMention, penalties, group.names, m => group.types(m.entityType), group.task2roles)
    }
    val bestNone = bestNoneState(relMention, penalties)
    //todo: process
    val bestProcesses = EventSpecs.processGroups.map {
      group =>
        bestProcess(relMention, penalties, group.names, m => group.types(m.entityType))
    }
    //best process

    val bestAll = (Seq(bestNone) ++ bestRegulations ++ bestNonRegulations ++ bestProcesses).maxByDouble(_.score)
    bestAll

  }

}

trait TriggerAwareCoordinatedThemeConsistency extends CanAverageParameters with StandardModule with ThemeModule {
  self =>

  import BioNLPConstants._

  type Role = RelationMentionArgument#Role

  val triggerScorer = new TriggerScorer {
    def useAverage: Boolean = self.useAverage
  }

  val argumentScorer = new ArgFeatureScorer {
    def useAverage: Boolean = self.useAverage

    override def labelFeatures(label: Any): scala.Iterable[(Any, Double)] = {
      label match {
        case (trigger, role) => //Seq(role -> 1.0)
          if (trigger.toString.endsWith("egulation"))
            Seq(role, trigger -> role, "reg" -> role).map(_ -> 1.0)
          else
            Seq(role, trigger -> role, "nonreg" -> role).map(_ -> 1.0)
      }
    }


    override def explain(variable: RelationMentionArgument#Role, value: Any): String = {
      ""
    }
  }


  def scoreTrigger(label: RelationMention#Label, penalties: Penalties, value: String): Double = {
    penalties.penalty(label, value) + triggerScorer.score(label, value)
  }

  class Cluster {
    val args = new ArrayBuffer[Role]

    def set(state: MutableState, value: String) = {
      for (arg <- args) {
        state(arg) = value
      }
    }

    def isEntityMention: Boolean = args.head.owner.argIsEntityMention
  }

  def scoreArgument(label: Role, penalties: Penalties, trigger: String, value: String): Double = {
    penalties.penalty(label, value) + argumentScorer.score(label, trigger -> value)
  }

  def scoreArgument(cluster: Cluster, penalties: Penalties, trigger: String, value: String): Double = {
    var result = 0.0
    for (label <- cluster.args) {
      result += scoreArgument(label, penalties, trigger, value)
    }
    result
  }

  def bestNoneState(relMention: RelationMention, penalties: Penalties): State = {
    val bestNone = new MutableState
    bestNone(relMention.label) = None
    bestNone.score = scoreTrigger(relMention.label, penalties, None)
    for (arg <- relMention.argumentCandidates) {
      val noneScore = scoreArgument(arg.role, penalties, None, None)
      bestNone(arg.role) = None
      bestNone.score += noneScore
    }
    bestNone
  }

  val clusterCache = new HashMap[RelationMention, Seq[Cluster]]

  def determineClusters(relMention: RelationMention): Seq[Cluster] = {
    val deps = relMention.sentence.getDependencyStructure("mcclosky")
    val clusters = new ArrayBuffer[Cluster]
    for (arg <- relMention.argumentCandidates) {
      var found = false
      for (existing <- clusters; if (!found)) {
        for (other <- existing.args; if (!found)) {
          val arg1 = arg.arg
          val arg2 = other.owner.arg
          if (arg.arg.getClass == other.owner.arg.getClass) {
            if (Conf.get("coordinateConj", false)) {

              val edges = deps.edges(arg1.head, arg2.head) ++ deps.edges(other.owner.arg.head, arg.arg.head)
              if (edges.exists(_.label.startsWith("conj"))) {
                found = true
                existing.args += arg.role
              }
            }
            if (Conf.get("coordinateHyphens", false)) {
              if (arg1.isInstanceOf[EntityMention] && arg2.isInstanceOf[EntityMention] &&
                (arg1.begin.prevOption == Some(arg2.end) && arg1.begin.word.startsWith("-") ||
                  arg2.begin.prevOption == Some(arg1.end) && arg2.begin.word.startsWith("-"))) {
                found = true
                existing.args += arg.role
              }
            }
            if (Conf.get("coordinateSlashes", false)) {
              if (arg1.isInstanceOf[EntityMention] && arg2.isInstanceOf[EntityMention] &&
                (arg1.begin.prevOption == Some(arg2.end) && arg1.begin.word.startsWith("/") ||
                  arg2.begin.prevOption == Some(arg1.end) && arg2.begin.word.startsWith("/"))) {
                found = true
                existing.args += arg.role
              }
            }
          }

        }
      }
      if (!found) {
        val cluster = new Cluster
        cluster.args += arg.role
        clusters += cluster
      }
    }
    //println("Clusters\n:%s".format(clusters.map(_.args.mkString(",")).mkString("\n")))
    clusters
  }

  def clusters(relMention: RelationMention): Seq[Cluster] = {
    clusterCache.getOrElseUpdate(relMention, determineClusters(relMention))
  }


  def bestState(relMention: RelationMention, penalties: Penalties,
                label: String): State = {
    val bestEvent = new MutableState
    bestEvent(relMention.label) = label
    bestEvent.score = scoreTrigger(relMention.label, penalties, label)
    val clusters = this.clusters(relMention)
    val isRegulation = label.endsWith("egulation")
    var (argMax, max) = (clusters.head, Double.NegativeInfinity)
    var themeCount = 0
    for (cluster <- clusters) {
      val noneScore = scoreArgument(cluster, penalties, label, None)
      if (!cluster.isEntityMention && !isRegulation) {
        cluster.set(bestEvent, None)
        bestEvent.score += noneScore
      } else {
        val causeScore = if (isRegulation) scoreArgument(cluster, penalties, label, Cause) else Double.NegativeInfinity
        val themeScore = scoreArgument(cluster, penalties, label, Theme)
        if (causeScore > Math.max(noneScore, themeScore)) {
          cluster.set(bestEvent, Cause)
          bestEvent.score += causeScore
        } else if (themeScore > Math.max(noneScore, causeScore)) {
          cluster.set(bestEvent, Theme)
          bestEvent.score += themeScore
          themeCount += 1
        } else {
          cluster.set(bestEvent, None)
          bestEvent.score += noneScore
        }
        if (themeScore - Math.max(causeScore, noneScore) > max) {
          max = themeScore - Math.max(causeScore, noneScore)
          argMax = cluster
        }
      }
    }
    if (themeCount == 0) {
      argMax.set(bestEvent, Theme)
      bestEvent.score += max
    }
    bestEvent

  }

  def updateWeights(obs: RelationMention, gold: State, guess: State, param: TrainingParameters) {
    triggerScorer.batchUpdateWeights(Seq(Example(obs.label, gold.value(obs.label), guess.value(obs.label))), param)
    val argTriggerExamples = for (arg <- obs.argumentCandidates) yield {
      Example(arg.role.asInstanceOf[Role],
        gold.value(obs.label) -> gold.value(arg.role), guess.value(obs.label) -> guess.value(arg.role))
    }
    argumentScorer.batchUpdateWeights(argTriggerExamples, param)
    //    println(guess)
    //println(argumentScorer.weightsString)
    //    triggerScorer.updateWeights(obs.label, gold.value(obs.label), guess.value(obs.label), param)
    //    for (arg <- obs.argumentCandidates) {
    //      argumentScorer.updateWeights(arg.role, gold.value(arg.role), guess.value(arg.role), param)
    //    }
  }

  def inferMAP(relMention: RelationMention, penalties: Penalties) = {

    val bestNone = bestNoneState(relMention, penalties)
    val bestEvents = for (eventType <- types.filter(_ != None)) yield bestState(relMention, penalties, eventType)

    val bestAll = (Seq(bestNone) ++ bestEvents).maxByDouble(_.score)
    bestAll

  }

}

trait TriggerAwareCoordinatedThemeConsistency2 extends CanAverageParameters with StandardModule with ThemeModule {
  self =>

  import BioNLPConstants._

  type Role = RelationMentionArgument#Role

  val triggerScorer = new TriggerScorer {
    def useAverage: Boolean = self.useAverage
  }

  val argumentScorer = new ArgFeatureScorer {
    def useAverage: Boolean = self.useAverage
  }


  override def loadWeights(prefix: String) = {
    super.loadWeights(prefix)
    //todo: load/store argument trigger scorer.
  }
  override def storeWeights(prefix: String) = {
    super.storeWeights(prefix)
  }
  val argumentTriggerScorer = new ArgFeatureScorer {
    def useAverage: Boolean = self.useAverage

    override def labelFeatures(label: Any): scala.Iterable[(Any, Double)] = {
      label match {
        case (trigger, role) => //Seq(role -> 1.0)
          if (trigger.toString.endsWith("egulation"))
            Seq(trigger -> role, "reg" -> role).map(_ -> 1.0)
          else
            Seq(trigger -> role, "nonreg" -> role).map(_ -> 1.0)
      }
    }


    override def explain(variable: RelationMentionArgument#Role, value: Any): String = {
      ""
    }
  }


  override def toString: String = {

    triggerScorer.weightsString
  }

  def scoreTrigger(label: RelationMention#Label, penalties: Penalties, value: String): Double = {
    penalties.penalty(label, value) + triggerScorer.score(label, value)
  }

  class Cluster {
    val args = new ArrayBuffer[Role]

    def set(state: MutableState, value: String) = {
      for (arg <- args) {
        state(arg) = value
      }
    }

    def isEntityMention: Boolean = args.head.owner.argIsEntityMention
  }

  def scoreArgument(label: Role, penalties: Penalties, trigger: String, value: String): Double = {
    penalties.penalty(label, value) +
      argumentScorer.score(label, value) +
      argumentTriggerScorer.score(label, trigger -> value)
  }

  def scoreArgument(cluster: Cluster, penalties: Penalties, trigger: String, value: String): Double = {
    var result = 0.0
    for (label <- cluster.args) {
      result += scoreArgument(label, penalties, trigger, value)
    }
    result
  }

  def bestNoneState(relMention: RelationMention, penalties: Penalties): State = {
    val bestNone = new MutableState
    bestNone(relMention.label) = None
    bestNone.score = scoreTrigger(relMention.label, penalties, None)
    for (arg <- relMention.argumentCandidates) {
      val noneScore = scoreArgument(arg.role, penalties, None, None)
      bestNone(arg.role) = None
      bestNone.score += noneScore
    }
    bestNone
  }

  val clusterCache = new HashMap[RelationMention, Seq[Cluster]]

  def determineClusters(relMention: RelationMention): Seq[Cluster] = {
    val deps = relMention.sentence.getDependencyStructure("mcclosky")
    val clusters = new ArrayBuffer[Cluster]
    for (arg <- relMention.argumentCandidates) {
      var found = false
      for (existing <- clusters; if (!found)) {
        for (other <- existing.args; if (!found)) {
          val arg1 = arg.arg
          val arg2 = other.owner.arg
          if (arg.arg.getClass == other.owner.arg.getClass) {
            if (Conf.get("coordinateConj", false)) {

              val edges = deps.edges(arg1.head, arg2.head) ++ deps.edges(other.owner.arg.head, arg.arg.head)
              if (edges.exists(_.label.startsWith("conj"))) {
                found = true
                existing.args += arg.role
              }
            }
            if (Conf.get("coordinateHyphens", false)) {
              if (arg1.isInstanceOf[EntityMention] && arg2.isInstanceOf[EntityMention] &&
                (arg1.begin.prevOption == Some(arg2.end) && arg1.begin.word.startsWith("-") ||
                  arg2.begin.prevOption == Some(arg1.end) && arg2.begin.word.startsWith("-"))) {
                found = true
                existing.args += arg.role
              }
            }
            if (Conf.get("coordinateSlashes", false)) {
              if (arg1.isInstanceOf[EntityMention] && arg2.isInstanceOf[EntityMention] &&
                (arg1.begin.prevOption == Some(arg2.end) && arg1.begin.word.startsWith("/") ||
                  arg2.begin.prevOption == Some(arg1.end) && arg2.begin.word.startsWith("/"))) {
                found = true
                existing.args += arg.role
              }
            }
          }

        }
      }
      if (!found) {
        val cluster = new Cluster
        cluster.args += arg.role
        clusters += cluster
      }
    }
    //println("Clusters\n:%s".format(clusters.map(_.args.mkString(",")).mkString("\n")))
    clusters
  }

  def clusters(relMention: RelationMention): Seq[Cluster] = {
    clusterCache.getOrElseUpdate(relMention, determineClusters(relMention))
  }


  def bestState(relMention: RelationMention, penalties: Penalties,
                label: String): State = {
    val bestEvent = new MutableState
    bestEvent(relMention.label) = label
    bestEvent.score = scoreTrigger(relMention.label, penalties, label)
    val clusters = this.clusters(relMention)
    val isRegulation = label.endsWith("egulation")
    var (argMax, max) = (clusters.head, Double.NegativeInfinity)
    var themeCount = 0
    for (cluster <- clusters) {
      val noneScore = scoreArgument(cluster, penalties, label, None)
      if (!cluster.isEntityMention && !isRegulation) {
        cluster.set(bestEvent, None)
        bestEvent.score += noneScore
      } else {
        val causeScore = if (isRegulation) scoreArgument(cluster, penalties, label, Cause) else Double.NegativeInfinity
        val themeScore = scoreArgument(cluster, penalties, label, Theme)
        if (causeScore > Math.max(noneScore, themeScore)) {
          cluster.set(bestEvent, Cause)
          bestEvent.score += causeScore
        } else if (themeScore > Math.max(noneScore, causeScore)) {
          cluster.set(bestEvent, Theme)
          bestEvent.score += themeScore
          themeCount += 1
        } else {
          cluster.set(bestEvent, None)
          bestEvent.score += noneScore
        }
        if (themeScore - Math.max(causeScore, noneScore) > max) {
          max = themeScore - Math.max(causeScore, noneScore)
          argMax = cluster
        }
      }
    }
    if (themeCount == 0) {
      argMax.set(bestEvent, Theme)
      bestEvent.score += max
    }
    bestEvent

  }

  def updateWeights(obs: RelationMention, gold: State, guess: State, param: TrainingParameters) {
    triggerScorer.batchUpdateWeights(Seq(Example(obs.label, gold.value(obs.label), guess.value(obs.label))), param)
    val argExamples = for (arg <- obs.argumentCandidates) yield {
      Example(arg.role.asInstanceOf[Role], gold.value(arg.role), guess.value(arg.role))
    }
    argumentScorer.batchUpdateWeights(argExamples, param)
    val argTriggerExamples = for (arg <- obs.argumentCandidates) yield {
      Example(arg.role.asInstanceOf[Role],
        gold.value(obs.label) -> gold.value(arg.role), guess.value(obs.label) -> guess.value(arg.role))
    }
    argumentTriggerScorer.batchUpdateWeights(argTriggerExamples, param)

    //    println(guess)
    //println(argumentScorer.weightsString)
    //    triggerScorer.updateWeights(obs.label, gold.value(obs.label), guess.value(obs.label), param)
    //    for (arg <- obs.argumentCandidates) {
    //      argumentScorer.updateWeights(arg.role, gold.value(arg.role), guess.value(arg.role), param)
    //    }
  }

  def inferMAP(relMention: RelationMention, penalties: Penalties) = {

    val bestNone = bestNoneState(relMention, penalties)
    val bestEvents = for (eventType <- types.filter(_ != None)) yield bestState(relMention, penalties, eventType)

    val bestAll = (Seq(bestNone) ++ bestEvents).maxByDouble(_.score)
    bestAll

  }

}


trait TriggerAwareThemeConsistency extends ThemeConsistency with CanAverageParameters with StandardModule {

  import BioNLPConstants._


  def bestNoneState(relMention: RelationMention, penalties: Penalties, prefix: String): State = {
    val bestNone = new MutableState
    bestNone(relMention.label) = None
    bestNone.score = scoreTrigger(relMention.label, penalties, None)
    for (arg <- relMention.argumentCandidates) {
      val noneScore = scoreArgument(arg.role, penalties, prefix + None)
      bestNone(arg.role) = None
      bestNone.score += noneScore
    }
    bestNone
  }

  def bestState(relMention: RelationMention, penalties: Penalties,
                labelFilter: String => Boolean,
                argFilter: RelationMentionArgument => Boolean,
                prefix: String): State = {
    val bestEvent = new MutableState
    //todo: all the scores can be precomputed in the inferMAP method
    val bestLabel = BioNLPConstants.types.filter(labelFilter(_)).
      maxByDouble(scoreTrigger(relMention.label, penalties, _))
    bestEvent(relMention.label) = bestLabel
    val isRegulation = bestLabel.endsWith("egulation")
    bestEvent.score = scoreTrigger(relMention.label, penalties, bestLabel)
    var (argMax, max) = (relMention.argumentCandidates.head.role, Double.NegativeInfinity)
    var themeCount = 0
    for (arg <- relMention.argumentCandidates) {
      val noneScore = scoreArgument(arg.role, penalties, None)
      if (!argFilter(arg)) {
        bestEvent(arg.role) = None
        bestEvent.score += noneScore
      } else {
        val causeScore = if (isRegulation) scoreArgument(arg.role, penalties, prefix + Cause) else Double.NegativeInfinity
        val themeScore = scoreArgument(arg.role, penalties, prefix + Theme)
        if (causeScore > Math.max(noneScore, themeScore)) {
          bestEvent(arg.role) = Cause
          bestEvent.score += causeScore
        } else if (themeScore > Math.max(noneScore, causeScore)) {
          bestEvent(arg.role) = Theme
          bestEvent.score += themeScore
          themeCount += 1
        } else {
          bestEvent(arg.role) = None
          bestEvent.score += noneScore
        }
        if (themeScore - Math.max(causeScore, noneScore) > max) {
          max = themeScore - Math.max(causeScore, noneScore)
          argMax = arg.role
        }
      }
    }
    if (themeCount == 0) {
      bestEvent(argMax) = Theme
      bestEvent.score += max
    }
    bestEvent

  }


  override def bestRegulationState(relMention: RelationMention, penalties: Penalties): State = {
    bestState(relMention, penalties, _.endsWith("egulation"), m => true, "reg:")
  }

  override def bestNonRegulationState(relMention: RelationMention, penalties: Penalties): State = {
    bestState(relMention, penalties, label => label != None && !label.endsWith("egulation"), _.argIsEntityMention, "normal:")
  }

  def triggerRolePrefix(eventType: String, role: String) = eventType match {
    case None => role
    case _ if (role == None) => None
    case t if (t.endsWith("egulation")) => "reg:" + role
    case _ => "normal:" + role
  }

  override def updateWeights(obs: RelationMention, gold: State, guess: State, param: TrainingParameters) {
    val goldType = gold.value(obs.label)
    val guessType = guess.value(obs.label)
    triggerScorer.batchUpdateWeights(Seq(Example(obs.label, goldType, guessType)), param)
    val argExamples = for (arg <- obs.argumentCandidates) yield {
      Example(arg.role.asInstanceOf[Role],
        triggerRolePrefix(goldType, gold.value(arg.role)), triggerRolePrefix(guessType, guess.value(arg.role)))
    }
    argumentScorer.batchUpdateWeights(argExamples, param)
  }

  override def inferMAP(relMention: RelationMention, penalties: Penalties) = {

    val bestRegulation = bestRegulationState(relMention, penalties)
    val bestNonRegulation = bestNonRegulationState(relMention, penalties)
    val bestNone = bestNoneState(relMention, penalties, "")

    val bestAll = Seq(bestNone, bestRegulation, bestNonRegulation).maxByDouble(_.score)
    bestAll

  }

}


/**
 * Maximizes local scores for trigger and all incoming arguments (from other events).
 * Makes sure that if the trigger label is "None", there should be no incoming edges.
 * Likewise, if there is an incoming edge the label must not be "None".
 */
trait ArgumentConsistency extends StandardModule with CanAverageParameters {

  import BioNLPConstants._

  type Observation = RelationMention

  type Role = RelationMentionArgument#Role

  val triggerScorer = new TriggerScorer {
    def useAverage: Boolean = ArgumentConsistency.this.useAverage
  }
  val argumentScorer = new ArgFeatureScorer {
    def useAverage: Boolean = ArgumentConsistency.this.useAverage
  }

  override def loadWeights(prefix: String) {
    val triggerIs = Util.getStreamFromFileOrClassPath(prefix + "/trigger.weights")
    triggerScorer.weights.load(triggerIs)
    val argIs = Util.getStreamFromFileOrClassPath(prefix + "/arg.weights")
    argumentScorer.weights.load(argIs)
  }
  override def storeWeights(prefix: String)  {
    val triggerOut = new FileOutputStream(createDirs(prefix + "/trigger.weights"))
    triggerScorer.weights.store(triggerOut)
    val argOut = new FileOutputStream(createDirs(prefix + "/arg.weights"))
    argumentScorer.weights.store(argOut)
  }


  override def weightsUpdated(obs: Observation): Unit = {
    triggerScorer.clearCache
    argumentScorer.clearCache
  }

  override def features(obs: Observation, state: State) = {
    val triggerFeats = triggerScorer.globalFeatures(obs.label, state(obs.label))
    val argFeats = new FeatureVector
    for (arg <- obs.argumentInCandidates) {
      argFeats.add(argumentScorer.globalFeatures(arg.role, state(arg.role)), 1.0)
    }
    new GlobalFeatureVector(Map(triggerScorer -> triggerFeats, argumentScorer -> argFeats))
  }

  override def featureDelta(obs: Observation, gold: State, guess: State) = {
    val triggerFeats = new FeatureVector
    if (gold(obs.label) != guess(obs.label)) {
      triggerFeats.add(triggerScorer.globalFeatures(obs.label, gold(obs.label)), 1.0)
      triggerFeats.add(triggerScorer.globalFeatures(obs.label, guess(obs.label)), -1.0)
    }
    val argFeats = new FeatureVector
    for (arg <- obs.argumentInCandidates; if (gold(arg.role) != guess(arg.role))) {
      argFeats.add(argumentScorer.globalFeatures(arg.role, gold(arg.role)), 1.0)
      argFeats.add(argumentScorer.globalFeatures(arg.role, guess(arg.role)), -1.0)
    }
    new GlobalFeatureVector(Map(triggerScorer -> triggerFeats, argumentScorer -> argFeats))
  }


  def scoreTrigger(label: RelationMention#Label, penalties: Penalties, value: String): Double = {
    penalties.penalty(label, value) + triggerScorer.score(label, value)
  }

  def scoreArgument(label: RelationMentionArgument#Role, penalties: Penalties, value: String): Double = {
    penalties.penalty(label, value) + argumentScorer.score(label, value)
  }

  def updateWeights(obs: RelationMention, gold: State, guess: State, param: TrainingParameters) {
    triggerScorer.batchUpdateWeights(Seq(Example(obs.label, gold.value(obs.label), guess.value(obs.label))), param)
    val argExamples = for (arg <- obs.argumentCandidates) yield {
      Example(arg.role.asInstanceOf[Role], gold.value(arg.role), guess.value(arg.role))
    }
    argumentScorer.batchUpdateWeights(argExamples, param)
    //    triggerScorer.updateWeights(obs.label, gold.value(obs.label), guess.value(obs.label), param)
    //    for (arg <- obs.argumentCandidates) {
    //      argumentScorer.updateWeights(arg.role, gold.value(arg.role), guess.value(arg.role), param)
    //    }
  }

  override def weights(obs: Observation) = {
    new GlobalFeatureVector(
      Map(triggerScorer -> triggerScorer.weights, argumentScorer -> argumentScorer.weights))
  }

  override def avgWeights(obs: Observation) = {
    new GlobalFeatureVector(
      Map(triggerScorer -> triggerScorer.averagingWeights, argumentScorer -> argumentScorer.averagingWeights))
  }

  //  def updateWeights(obs: RelationMention, gold: State, guess: State, param: TrainingParameters) {
  //    triggerScorer.updateWeights(obs.label, gold.value(obs.label), guess.value(obs.label), param)
  //    for (arg <- obs.containedInCandidates.map(_.value)) {
  //      argumentScorer.updateWeights(arg.role, gold.value(arg.role), guess.value(arg.role), param)
  //    }
  //  }

  def bestNoneState(relMention: RelationMention, penalties: Penalties): State = {
    val bestNone = new MutableState
    bestNone(relMention.label) = None
    bestNone.score = scoreTrigger(relMention.label, penalties, None)
    for (arg <- relMention.containedInCandidates.map(_.value)) {
      val noneScore = scoreArgument(arg.role.asInstanceOf[Role], penalties, None)
      bestNone(arg.role) = None
      bestNone.score += noneScore
    }
    bestNone
  }

  val checkSelfRegulation = Conf.get("checkSelfRegulation", true)

  /**
   * Finds a state where event is not "None", and there is at least one argument
   */
  def bestEventState(relMention: RelationMention, penalties: Penalties): State = {
    val bestEvent = new MutableState
    val label = EventSpecs.events.filter(_ != None).maxByDouble(scoreTrigger(relMention.label, penalties, _))
    bestEvent(relMention.label) = label
    val triggerScore = scoreTrigger(relMention.label, penalties, label.toString)
    bestEvent.score = triggerScore
    var selfArg: (RelationMentionArgument, String, Double) = null
    for (arg <- relMention.containedInCandidates.map(_.value)) {
      val causeScore = scoreArgument(arg.role.asInstanceOf[Role], penalties, Cause)
      val themeScore = scoreArgument(arg.role.asInstanceOf[Role], penalties, Theme)
      val noneScore = scoreArgument(arg.role.asInstanceOf[Role], penalties, None)
      val (role, score) = if (causeScore > Math.max(noneScore, themeScore)) {
        bestEvent(arg.role) = Cause
        bestEvent.score += causeScore
        Cause -> causeScore
      } else if (themeScore > Math.max(noneScore, causeScore)) {
        bestEvent(arg.role) = Theme
        bestEvent.score += themeScore
        Theme -> themeScore
      } else {
        bestEvent(arg.role) = None
        bestEvent.score += noneScore
        None -> noneScore
      }
      //we can't have self regulation edges to regulations
      if (checkSelfRegulation && arg.owner.head == relMention.head && role != None && EventSpecs.regulations(label)) {
        selfArg = (arg.asInstanceOf[RelationMentionArgument], role, score - noneScore)
        bestEvent(arg.role) = None
        bestEvent.score += noneScore - score
      }
    }
    if (checkSelfRegulation && selfArg != null) {
      val (arg,role,scoreDelta) = selfArg
      val noReg = EventSpecs.events.filter(l => l != None && !EventSpecs.regulations(l)).maxByDouble(scoreTrigger(relMention.label, penalties, _))
      val noRegScore = scoreTrigger(relMention.label, penalties, noReg.toString)
      if (scoreDelta > triggerScore - noRegScore) {
        bestEvent(arg.role) = role
        bestEvent(relMention.label) = noReg
        bestEvent.score += scoreDelta + noRegScore - triggerScore
      }
    }
    bestEvent

  }


  def inferMAP(relMention: RelationMention, penalties: Penalties) = {
    val bestEvent = bestEventState(relMention, penalties)
    val bestNone = bestNoneState(relMention, penalties)

    if (bestEvent.score > bestNone.score) bestEvent else bestNone
  }


}


