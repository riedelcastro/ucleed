package cc.refectorie.proj.bionlp2011

import cc.factorie._
import cc.factorie.la.{SingletonBinaryVector, SparseHashVector}
import cc.refectorie.proj.factorieie.data._
import cc.refectorie.proj.factorieie.template._
import cc.refectorie.proj.factorieie.util.Util
import java.io.FileOutputStream

/**
 * @author sriedel
 */
trait BioNLPModule extends Composed with FullyFactorized with MakeNoUpdates {
  type Observation = Iterable[Document]


  override def storeWeights(prefix: String) = {
    docModule.storeWeights(prefix + "/doc")
  }
  override def loadWeights(prefix: String) = {
    docModule.loadWeights(prefix + "/doc")
  }

  def docModule: DocModule

  def decompose(obs: Iterable[Document]) = obs.map(Assign(docModule, _))
}

trait DocModule extends Dispatch with StandardComposed {
  type Observation = Document

  override def storeWeights(prefix: String) = {
    sentenceModule.storeWeights(prefix + "/sentence")
  }
  override def loadWeights(prefix: String) = {
    sentenceModule.loadWeights(prefix + "/sentence")
  }

  def sentenceModule: Module {type Observation = Sentence}

  def dispatch(obs: Document) = Seq(
    Batch(sentenceModule, obs.sentences.filter(!_.entityMentionCandidates.isEmpty)))
}

trait SentenceModule extends Dispatch with DualDecomposed with StandardComposed {
  type Observation = Sentence

  def relationMentionModule: Module {type Observation = RelationMention}

  def relationMentionAsTargetModule: Module {type Observation = RelationMention}

  def dispatch(obs: Sentence) = Seq(
    Batch(relationMentionModule, obs.relationMentionCandidates),
    Batch(relationMentionAsTargetModule, obs.relationMentionCandidates))
}

trait MutableSentenceModule extends MutableDispatch with DualDecomposed with StandardComposed {
  type Observation = Sentence

}

trait EventArgSiblingModule extends SiblingBigramModule with TrainedByEnclosingModule with CanAverageParameters {
  self =>

  import BioNLPConstants._

  type Arg = RelationMentionArgument#Role
  type Observation = RelationMention

  val onlyEntitySiblings = Conf.get("onlyEntitySiblings", true)

  override def loadWeights(prefix: String) {
    val triggerIs = Util.getStreamFromFileOrClassPath(prefix + "/uni.weights")
    unigramScorer.weights.load(triggerIs)
    val argIs = Util.getStreamFromFileOrClassPath(prefix + "/bi.weights")
    bigramScorer.weights.load(argIs)
  }
  override def storeWeights(prefix: String) {
    val triggerOut = new FileOutputStream(createDirs(prefix + "/uni.weights"))
    unigramScorer.weights.store(triggerOut)
    val argOut = new FileOutputStream(createDirs(prefix + "/bi.weights"))
    bigramScorer.weights.store(argOut)

  }
  def orderedArgs(observation: RelationMention): Seq[Arg] = {
    val args = if (onlyEntitySiblings)
      observation.argumentCandidates.filter(_.argIsEntityMention)
    else
      observation.argumentCandidates
    args.sortBy(_.arg.head.indexInDocument).map(_.role)
  }




  val unigramScorer: LocalLinearScorer[Arg] = new ArgFeatureScorer {
    def useAverage: Boolean = self.useAverage
  }

  val bigramScorer: LocalLinearScorer[(Arg, Arg)] = new ArgPairFeatureScorer {
    def useAverage: Boolean = self.useAverage
  }
}

trait EventArgParentModule extends SiblingBigramModule with TrainedByEnclosingModule with CanAverageParameters {
  self =>

  import BioNLPConstants._

  override def loadWeights(prefix: String) {
    val triggerIs = Util.getStreamFromFileOrClassPath(prefix + "/uni.weights")
    unigramScorer.weights.load(triggerIs)
    val argIs = Util.getStreamFromFileOrClassPath(prefix + "/bi.weights")
    bigramScorer.weights.load(argIs)
  }
  override def storeWeights(prefix: String) {
    val triggerOut = new FileOutputStream(createDirs(prefix + "/uni.weights"))
    unigramScorer.weights.store(triggerOut)
    val argOut = new FileOutputStream(createDirs(prefix + "/bi.weights"))
    bigramScorer.weights.store(argOut)

  }

  type Arg = RelationMentionArgument#Role
  type Observation = SentenceMention[_]

  def orderedArgs(observation: Observation): Seq[Arg] =
  //    observation.argumentCandidates.sortBy(_.arg.head.indexInDocument).map(_.role)
    observation.containedInCandidates.map(_.value).sortBy(_.arg.head.indexInDocument).map(_.role.asInstanceOf[Arg])


  val unigramScorer: LocalLinearScorer[Arg] = new ArgFeatureScorer {
    def useAverage: Boolean = self.useAverage
  }

  val bigramScorer: LocalLinearScorer[(Arg, Arg)] = new ArgPairFeatureScorerForParents {
    def useAverage: Boolean = self.useAverage
  }
}


trait SentenceModule2 extends Dispatch with DualDecomposed with StandardComposed {
  type Observation = Sentence

  def relationMentionModule: Module {type Observation = RelationMention}

  def relationMentionAsTargetModule: Module {type Observation = RelationMention}

  def siblingModule: Option[EventArgSiblingModule]

  def parentModule: Option[EventArgParentModule]

  def asymmetryModule: Option[AsymmetryModule]

  def antiTransitivityModule: Option[AntiTransitivityModule]


  val onlyEntityParents = Conf.get("onlyEntityParents", true)

  def dispatch(obs: Sentence): Iterable[AbstractBatch] = {
    val s = Seq[AbstractBatch](
      Batch(relationMentionModule, obs.relationMentionCandidates),
      Batch(relationMentionAsTargetModule, obs.relationMentionCandidates))
    s ++ parentModule.map(Batch(_, if (onlyEntityParents) obs.entityMentionCandidates else obs.entityMentionCandidates ++ obs.relationMentionCandidates)).toSeq ++
      siblingModule.map(Batch(_, obs.relationMentionCandidates)).toSeq ++
      asymmetryModule.map(Batch(_, Seq(obs))).toSeq ++
      antiTransitivityModule.map(m => Batch(m, m.unroll(obs))).toSeq
  }

}

trait UpdatePolicy[Observation] {
  def updateWeights(obs: Observation, gold: State, guess: State, param: TrainingParameters): Unit
}

trait HasUpdatePolicy extends Module {

  def updatePolicy: UpdatePolicy[Observation]

  def updateWeights(obs: Observation, gold: State, guess: State, param: TrainingParameters): Unit = {
    updatePolicy.updateWeights(obs, gold, guess, param)
  }

}

trait SentenceModule3 extends Dispatch with OnDemandDualDecomposed with AntiTransivityFinder with GlobalLearner {
  def relationMentionModule: Module {type Observation = RelationMention}

  def relationMentionAsTargetModule: Option[Module {type Observation = RelationMention}]

  def siblingModule: Option[EventArgSiblingModule]

  def parentModule: Option[EventArgParentModule]

  def asymmetryModule: Option[AsymmetryModule]

  val onlyEntityParents = Conf.get("onlyEntityParents", true)

  def dispatch(obs: Sentence): Iterable[AbstractBatch] = {
    val s = Seq[AbstractBatch](Batch(relationMentionModule, obs.relationMentionCandidates))
    s ++ relationMentionAsTargetModule.map(Batch(_, obs.relationMentionCandidates)).toSeq ++
      parentModule.map(Batch(_, if (onlyEntityParents) obs.entityMentionCandidates else obs.entityMentionCandidates ++ obs.relationMentionCandidates)).toSeq ++
      siblingModule.map(Batch(_, obs.relationMentionCandidates)).toSeq ++
      asymmetryModule.map(Batch(_, Seq(obs))).toSeq
  }

}


trait ComposedRelationMentionModule extends RelationMentionModule with Dispatch {
  def triggerModule: TriggerModule

  def argumentModule: ArgumentModule

  def themeConsistency: ThemeConsistency

  def dispatch(obs: RelationMention) = Seq(
    Batch(triggerModule, Seq(obs)),
    Batch(argumentModule, obs.argumentCandidates),
    Batch(themeConsistency, Seq(obs)))

}


trait RelationMentionModule {
  type Observation = RelationMention

}

trait LocalLinearScorerModule extends Module {
  def scorer: LinearScorer[Observation]

  override def features(obs: Observation, state: State): GlobalFeatureVector = {
    new GlobalFeatureVector(Map(scorer -> scorer.globalFeatures(obs, state(obs))))
  }

  override def weights(obs: Observation): GlobalFeatureVector = {
    new GlobalFeatureVector(Map(scorer -> scorer.weights))
  }

  override def avgWeights(obs: Observation): GlobalFeatureVector = {
    new GlobalFeatureVector(Map(scorer -> scorer.averagingWeights))
  }

  def domain: Seq[Any]

  def inferMAP(observation: Observation, penalties: Penalties) = {
    val result = new MutableState
    val scored = for (d <- domain) yield {
      d -> (scorer.score(observation, d) + penalties.penalty(observation, d))
    }
    val argMax = scored.maxByDouble(_._2)
    result(observation) = argMax._1
    result.score = argMax._2
    result
  }


  override def score(obs: Observation, state: State) = {
    scorer.score(obs, state(obs))
  }

  override def featureDelta(obs: Observation, gold: State, guess: State): GlobalFeatureVector = {
    if (gold(obs) == guess(obs)) GlobalFeatureVector.empty
    else {
      val delta = new FeatureVector
      delta.add(scorer.globalFeatures(obs, gold(obs)), 1.0)
      delta.add(scorer.globalFeatures(obs, guess(obs)), -1.0)
      new GlobalFeatureVector(Map(scorer -> delta))
    }
  }


}


trait LocalModule extends Module {
  type Label <: LabelVariable[String]
  type Vector <: BinaryFeatureVectorVariable[_]

  def label(obs: Observation): Label

  def vector(obs: Observation): Vector

  def template: Template1[Label] with DotStatistics2[Label, Vector]

  def toVector(observation: Observation, label: String): la.Vector = {
    val vector1 = TemplateHelper.toVector(this.label(observation), label)
    val vector2 = vector(observation).vector
    vector1 flatOuter vector2
  }

  def updateWeights(observation: Observation, towards: State, awayFrom: State, param: TrainingParameters) = {
    val t = towards.value(label(observation))
    val a = awayFrom.value(label(observation))
    val towardsVector = new SparseHashVector(100)
    towardsVector += toVector(observation, t)
    val awayFromVector = new SparseHashVector(100)
    awayFromVector += toVector(observation, a)
    template.weights += towardsVector * param.learningRate
    template.weights += awayFromVector * (-param.learningRate)

  }

  def inferMAP(observation: Observation, penalties: Penalties) = {
    //iterate over all relation mention variables and greedily pick
    //highest scoring state
    val l = label(observation)
    val state = new MutableState
    val features = vector(observation).vector
    var (max, argMax) = (Double.NegativeInfinity, l.domain(0))
    for (value <- l.domain) {
      val vector = TemplateHelper.toVector(l, value) flatOuter features
      val score = vector dot template.weights
      val penalized = score + penalties.penalty(l, value)
      if (penalized > max) {
        max = penalized
        argMax = value
      }
    }
    state(l) = argMax
    state.score = max
    state
  }


}

object BioNLPUtils {
  def headFinder(begin: Token, end: Token): Token = {
    (for (depStructure: DependencyStructure <- Some(begin.sentence.getDependencyStructure("mcclosky"))) yield {
      var current = begin
      var head: Token = null
      while (current.indexInDocument <= end.indexInDocument) {
        val heads = depStructure.heads(current).map(_.head.indexInDocument)
        if (current.tag != "CD" &&
          (heads.exists(index => index < begin.indexInDocument || index > end.indexInDocument))) {
          head = current
        }
        current = current.next
      }
      if (head == null) end else head
    }).getOrElse(end)
  }

  def fileName(docID: String): String = {
    def orElseLength(n: Int) = if (n > -1) n else docID.length()
    docID.substring(docID.lastIndexOf('/') + 1, orElseLength(docID.lastIndexOf('.')))
  }

}

object BioNLPConstants {
  val Binding = "Binding"

  val Transcription = "Transcription"

  val ProteinCatabolism = "Protein_catabolism"

  val Localization = "Localization"

  val None = "None"

  val GeneExpression = "Gene_expression"

  val Regulation = "Regulation"

  val PositiveRegulation = "Positive_regulation"

  val NegativeRegulation = "Negative_regulation"

  val Phosphorylation = "Phosphorylation"

  val Theme = "Theme"

  val Cause = "Cause"

  val Participant = "Participant"


  val NoSite = "NoSite"

  val Entity = "Entity"
  val EntityTag = "entity"


  val types = Seq(Binding, Transcription, ProteinCatabolism, Localization,
    GeneExpression, Regulation, PositiveRegulation, NegativeRegulation, Phosphorylation, None)

  val roles = Seq(None, Theme, Cause)

}


trait TriggerModule extends LocalModule {
  type Observation = RelationMention
  type Label = RelationMention#Label
  type Vector = RelationMentionFeatureVector

  def label(obs: RelationMention): Label = obs.label

  def vector(obs: RelationMention) = new RelationMentionFeatureVector(obs)

  val template = new LocalRelationMentionLabelTemplate {
    override def freezeDomains: Unit = {}
  }.init
}


trait ArgumentModule extends LocalModule {
  type Observation = RelationMentionArgument
  type Label = RelationMentionArgument#Role
  type Vector = RelationMentionArgumentFeatureVector

  def label(obs: RelationMentionArgument): Label = obs.role

  def vector(obs: RelationMentionArgument) = new RelationMentionArgumentFeatureVector(obs)

  val template = new LocalRelationMentionArgumentRoleTemplate {
    override def freezeDomains: Unit = {}
  }.init
}

object TemplateHelper {
  def toVector[T1](v1: CategoricalVars[T1], t1: T1): la.Vector = {
    v1 match {
      case d: AbstractCategoricalVars => new SingletonBinaryVector(d.domain.size, d.domain.index(t1))
      case _ => error("Don't support variable type %s".format(v1))
    }
  }

}

class TransitiveClosure(self: State) extends State {

  import BioNLPConstants._

  def apply(variable: Any): Any = {
    variable match {
      case arg: RelationMentionArgument#Role => {
        val relMention: RelationMention = arg.owner.owner
        val role = self(arg).toString
        if (arg.owner.argIsEntityMention
          && role == None
          && self(relMention.label).toString.endsWith("egulation")) {
          val prot = arg.owner.entityMention
          //check if relMention has an event argument that has the prot as argument
          for (eventArg <- relMention.argumentCandidates.filter(_.argIsRelationMention);
               if (self(eventArg.role) != None)) {
            for (protArg <- eventArg.relationMention.argumentCandidates.filter(_.argIsEntityMention);
                 if (self(protArg.role) != None)) {
              if (protArg.entityMention == prot) {
                //                println("Added transitive closure")
                return self(protArg.role)
              }
            }
          }
          role
        } else
          role
      }
      case _ => self.apply(variable)
    }
  }

  def getValue[T](variable: CategoricalVars[T]): Option[T] = {
    try {
      Some(apply(variable).asInstanceOf[T])
    } catch {
      case _ => scala.None
    }
  }

  def mapping: scala.Iterable[(Any, Any)] = error("Unsupported")

  def score: Double = error("Not supported")

  def upperBound: Double = error("Not supported")

}