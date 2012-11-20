package cc.refectorie.proj.bionlp2011

import cc.factorie._

import collection.mutable.{ArrayBuffer, HashMap}
import cc.refectorie.proj.factorieie.util.{Timer, HasLogger,Counting}

/**
 * @author riedel
 */

class Penalties extends HashMap[(Any, Any), Double] {
  override def default(key: (Any, Any)): Double = 0.0

  def penalty(variable: Any, value: Any) = apply(variable -> value)

  def domain = keys

  def copy = {
    val result = new Penalties
    result ++= this
    result
  }

}

class HammingLoss(gold: State) extends Penalties {
  override def default(key: (Any, Any)): Double = {
    val (variable, value) = key
    if (value == gold(variable)) 0.0 else 1.0
  }

  override def copy = new HammingLoss(gold)
}

class FactorizedLoss(gold: State, loss: (Any, Any) => Double) extends Penalties {
  override def default(key: (Any, Any)): Double = {
    val (variable, value) = key
    loss(value, gold(variable))
  }

  override def copy = new HammingLoss(gold)
}


trait State {
  def apply(variable: Any): Any

  def value[T](variable: CategoricalVars[T]) = getValue(variable).get

  def getValue[T](variable: CategoricalVars[T]): Option[T]

  def mapping: Iterable[(Any, Any)]

  def score: Double

  def upperBound: Double

  def domain: scala.collection.Set[Any] = mapping.map(_._1).toSet

  override def toString = {
    domain.map(variable => "%-20s: %-10s".format(variable, apply(variable))).mkString("\n")
  }

  def real(variable: Any, value: Any) = {
    if (apply(variable) == value) 1.0 else 0.0
  }

}

class MutableState extends HashMap[Any, Any] with State {
  var score = 0.0

  var upperBound = 0.0

  def getValue[T](variable: CategoricalVars[T]) = get(variable).map(_.asInstanceOf[T])


  def mapping: scala.Iterable[(Any, Any)] = this


  override def domain = keySet
}

class MutableRealState(defaultState: Any) extends MutableState {
  val reals = new HashMap[Any, (Any, Double)]
  override def domain = (reals.keys ++ super.domain).toSet
  override def apply(key: Any) = {
    val nonReal = super.get(key)
    nonReal.getOrElse(reals.get(key).map(_._1).getOrElse(defaultState))
  }
  override def real(variable: Any, value: Any) = {
    reals.get(variable).map(pair => if (pair._1 == value) pair._2 else 0.0).getOrElse(super.real(variable, value))
  }
}


object TrueValueState extends TrueValueState
class TrueValueState extends State {
  def getValue[T](variable: CategoricalVars[T]): Option[T] = {
    variable match {
      case c: CategoricalVariableWithTrueSetting[_] => Some(c.trueValue.asInstanceOf[T])
      case _ => None
    }
  }

  def mapping: scala.Iterable[(Any, Any)] = error("Unsupported")

  def score: Double = error("Not supported")

  def className(variable: Any) = {
    variable match {
      case v: AnyRef => v.getClass.getName
      case _ => "?"
    }
  }

  def apply(variable: Any): Any = {
    variable match {
      case c: CategoricalVars[_] => value(c)
      case _ => error("No true setting for variable %s of type %s".format(variable, className(variable)))
    }
  }


  def upperBound: Double = error("Not supported")
}


case class TrainingParameters(learningRate: Double,
                              C: Double,
                              loss: (Any, Any) => Double = (gold, guess) => if (gold == guess) 0.0 else 1.0)


trait Module {
  type Observation

  def createDirs(filename:String) = {
    val file = new java.io.File(filename)
    file.getParentFile.mkdirs()
    filename
  }

  def storeWeights(prefix:String) {}
  def loadWeights(prefix:String) {}

  /**
   * Find the MAP state for the shared variables of this module, under the given penalties
   */
  def inferMAP(obs: Observation, penalties: Penalties = new Penalties): State

  /**
   * Find the MAP state for the shared variables of this module, under the given penalties
   */
  def nBest(n: Int, obs: Observation, penalties: Penalties = new Penalties): Seq[State] = Seq.empty


  /**
   * Update the model parameters towards the `towards` state, and away from the `awayFrom` state.
   */
  def updateWeights(obs: Observation, towards: State, awayFrom: State, param: TrainingParameters): Unit

  /**
   * Learn to predict the given gold state under the given penalties, and with the given training parameters.
   */
  def learn(obs: Observation, gold: State, penalties: Penalties, param: TrainingParameters): Unit

  /**
   * Assign a score to the given observation-solution pair. By default uses dot product of feature representation
   * and weights.
   */
  def score(obs: Observation, state: State): Double = 0.0
  /*{
   val scores = for ((key, feats) <- features(obs, state)) yield feats.dot(weights(obs)(key))
   scores.sum
 } */

  /**
   * Return an aggregation of feature representations for the given observation state. Can return
   * empty map.
   */
  def features(obs: Observation, state: State): GlobalFeatureVector = GlobalFeatureVector.empty

  /**
   * Calculates the difference of gold and guess feature. May be overriden for quicker implementation
   */
  def featureDelta(obs: Observation, gold: State, guess: State): GlobalFeatureVector = {
    val goldFeatures = features(obs, gold)
    val guessFeatures = features(obs, guess)
    val delta = new GlobalFeatureVector(goldFeatures.keys.map(_ -> new FeatureVector).toMap)
    delta.add(goldFeatures, 1.0)
    delta.add(guessFeatures, -1.0)
    delta
  }

  /**
   * A weight vector underlying this component. Can be empty.
   */
  def weights(obs: Observation): GlobalFeatureVector = GlobalFeatureVector.empty

  /**
   * An average weight vector underlying this component. Can be empty.
   */
  def avgWeights(obs: Observation): GlobalFeatureVector = GlobalFeatureVector.empty

  /**
   * The loss function that this module should use to learn from.
   */
  def lossFunction: LossFunction[Observation] = NoLoss

  /**
   * Called when external code changed weight vector
   */
  def weightsUpdated(obs: Observation): Unit = {}
}

trait LossFunction[-Observation] {
  def loss(obs: Observation, gold: State, guess: State): Double
}

object NoLoss extends LossFunction[Any] {
  def loss(obs: Any, gold: State, guess: State): Double = 0.0
}

trait GlobalLearner extends Module {
  def averaging: Boolean

  /**
   * Update the model parameters towards the `towards` state, and away from the `awayFrom` state.
   */
  def updateWeights(obs: Observation, gold: State, guess: State, param: TrainingParameters): Unit = {
    //    val goldFeatures = features(obs, gold)
    //    val guessFeatures = features(obs, guess)
    //    val delta = new GlobalFeatureVector(goldFeatures.keys.map(_ -> new FeatureVector).toMap)
    //    delta.add(goldFeatures, 1.0)
    //    delta.add(guessFeatures, -1.0)
    //    println("Update weights:")
    //    println(guess)
    val delta = featureDelta(obs, gold, guess)
    val weights = this.weights(obs)
    val scoreDelta = -delta.dot(weights)
    val loss = scoreDelta + lossFunction.loss(obs, gold, guess)
    val squaredNorm = delta.squaredNorm
    val learningRate = if (squaredNorm > 0) loss / squaredNorm else 0.0
    weights.add(delta, learningRate)
    if (averaging) {
      val avgWeights = this.avgWeights(obs)
      avgWeights.add(delta, learningRate)
    }
    weightsUpdated(obs)
  }

}


/**
 * Times calls to the main `Module` methods.
 */
trait TimedModule extends Module {
  val timer = new Timer

  abstract override def inferMAP(obs: Observation, penalties: Penalties): State = {
    timer.time("inferMAP", super.inferMAP(obs, penalties))
  }

  abstract override def learn(obs: Observation, gold: State, penalties: Penalties, param: TrainingParameters): Unit = {
    timer.time("learn", super.learn(obs, gold, penalties, param))
  }

  abstract override def updateWeights(obs: Observation, towards: State, awayFrom: State, param: TrainingParameters): Unit = {
    timer.time("updateWeights", super.updateWeights(obs, towards, awayFrom, param))
  }

}

/**
 * A solvable problem instance. We can consider this as a factor, and the problem observation
 * is an ID that identifies the factor.
 */
trait SubtaskAssignment {
  /**
   * Find the MAP state for the shared variables of this module, under the given penalties
   */
  def inferMAP(penalties: Penalties): State

  /**
   * Find the MAP state for the shared variables of this module, under the given penalties
   */
  def nBest(n: Int, penalties: Penalties): Seq[State]


  /**
   * Update the model parameters towards the `towards` state, and away from the `awayFrom` state.
   */
  def updateWeights(towards: State, awayFrom: State, param: TrainingParameters)

  /**
   * Learn to predict the given gold state under the given penalties, and with the given training parameters.
   */
  def learn(gold: State, penalties: Penalties, param: TrainingParameters)

  /**
   * The module assigned to this task
   */
  val module: Module

  def instance: module.Observation

  def features(state: State): GlobalFeatureVector

  def featureDelta(gold: State, guess: State): GlobalFeatureVector

  def weights: GlobalFeatureVector

  def avgWeights: GlobalFeatureVector

  def weightsUpdated: Unit

}

/**
 * A Composed module breaks up an observation into several problem instances which are solved in isolation.
 * The composed module takes these solutions, and integrates into its own solution (possibly more consistent).
 */
trait Composed extends Module {
  def decompose(obs: Observation): Iterable[SubtaskAssignment]

  override def features(obs: Observation, state: State) = {
    val pairs = for (task <- decompose(obs); (key, feats) <- task.features(state)) yield (task.module, key) -> feats
    val keys = pairs.map(_._1).toSet
    val key2features = keys.map(key => key -> pairs.filter(_._1 == key).map(_._2))
    val summed = for ((key, features) <- key2features) yield {
      key -> {
        if (features.size == 1) features.head
        else {
          val aggregated = new FeatureVector
          for (vector <- features) {
            aggregated.add(vector, 1.0)
          }
          aggregated
        }
      }
    }
    //need to sum up features
    new GlobalFeatureVector(summed.toMap)
  }

  override def featureDelta(obs: Observation, gold: State, guess: State) = {
    val result = new GlobalFeatureVector
    for (task <- decompose(obs)) {
      result.add(task.module, task.featureDelta(gold, guess), 1.0)
    }
    result
  }


  override def weights(obs: Observation): GlobalFeatureVector = {
    val pairs = for (task <- decompose(obs); (key, weights) <- task.weights) yield (task.module, key) -> weights
    new GlobalFeatureVector(pairs.toMap)
  }

  override def avgWeights(obs: Observation): GlobalFeatureVector = {
    val pairs = for (task <- decompose(obs); (key, weights) <- task.avgWeights) yield (task.module, key) -> weights
    new GlobalFeatureVector(pairs.toMap)
  }

  override def weightsUpdated(obs: Observation): Unit = {
    for (task <- decompose(obs)) task.weightsUpdated
  }


}

case class Assign[T](module: Module {type Observation = T}, instance: T) extends SubtaskAssignment {
  def learn(gold: State, penalties: Penalties, param: TrainingParameters) = {
    module.learn(instance, gold, penalties, param)
  }

  def updateWeights(towards: State, awayFrom: State, param: TrainingParameters) = {
    module.updateWeights(instance, towards, awayFrom, param)
  }

  def inferMAP(penalties: Penalties): State = {
    module.inferMAP(instance, penalties)
  }

  def nBest(n: Int, penalties: Penalties): Seq[State] = {
    module.nBest(n, instance, penalties)
  }


  def weights: GlobalFeatureVector = module.weights(instance)

  def avgWeights: GlobalFeatureVector = module.avgWeights(instance)


  def weightsUpdated: Unit = {
    module.weightsUpdated(instance)
  }

  def features(state: State): GlobalFeatureVector = module.features(instance, state)

  def featureDelta(gold: State, guess: State): GlobalFeatureVector = module.featureDelta(instance, gold, guess)

}

trait AbstractBatch {
  def tasks: Iterable[SubtaskAssignment]
}

case class Batch[T](module: Module {type Observation = T}, instances: Iterable[T]) extends AbstractBatch {
  def tasks: Iterable[SubtaskAssignment] = for (instance <- instances) yield Assign(module, instance)
}

trait Unroller extends Module {
  type Rolled

  def unroll(rolled: Rolled): Seq[Observation]
}

trait DefaultUnroller extends Unroller {
  type Rolled = Observation

  def unroll(rolled: Rolled) = Seq(rolled)
}

trait DynamicDispatch extends Dispatch {
  self =>

  def unrollers: Seq[Unroller {type Rolled = self.Observation}]

  def dispatch(obs: Observation) = for (unroller <- unrollers) yield
    new Batch[unroller.Observation](unroller, unroller.unroll(obs))
}

trait Dispatch extends Composed {
  def dispatch(obs: Observation): Iterable[AbstractBatch]

  def decompose(obs: Observation): Iterable[SubtaskAssignment] = {
    dispatch(obs).flatMap(_.tasks)
    //for (batch <- dispatch(obs); task <- batch.tasks) yield task
  }
}

trait MutableDispatch extends Dispatch {
  val dispatchers = new ArrayBuffer[Observation => Batch[_]]

  def dispatch(obs: Observation) = dispatchers.map(_.apply(obs))
}

trait SequentialLearning extends Composed with HasLogger {
  def learn(obs: Observation, gold: State, penalties: Penalties, param: TrainingParameters) = {
    val counting = new Counting(10, count => logger.info("Learned from %d instances".format(count)))
    for (task <- counting(decompose(obs)))
      task.learn(gold, penalties, param)
  }
}

trait RepeatedLearning extends Module with HasLogger {
  def epochs: Int

  abstract override def learn(obs: Observation, gold: State, penalties: Penalties, param: TrainingParameters) = {
    for (epoch <- 0 until epochs) {
      super.learn(obs, gold, penalties, param)
      logger.info("Finished epoch %d".format(epoch))
    }
  }
}

trait DecayingLearningRate extends Module {
  var calls = 1

  abstract override def learn(obs: Observation, gold: State, penalties: Penalties, param: TrainingParameters) {
    super.learn(obs, gold, penalties, param.copy(learningRate = param.learningRate / calls))
    calls += 1
  }
}

trait UpdateAllSubcomponents extends Composed {
  def updateWeights(obs: Observation, towards: State, awayFrom: State, param: TrainingParameters) {
    //println("Updating away from: " + awayFrom)
    for (task <- decompose(obs)) task.updateWeights(towards, awayFrom, param)
  }

}

trait MakeNoUpdates extends Module {
  def updateWeights(obs: Observation, towards: State, awayFrom: State, param: TrainingParameters) {

  }
}

trait StochasticLearning extends Composed {
  def learn(obs: Observation, gold: State, penalties: Penalties, param: TrainingParameters) = {
    val task = decompose(obs).sampleUniformly
    task.learn(gold, penalties, param)
  }
}

trait TrainedByEnclosingModule extends Module {
  def learn(obs: Observation, gold: State, penalties: Penalties, param: TrainingParameters) = {
  }
}

trait StandardModule extends Module with Perceptron
trait StandardComposed extends Composed with UpdateAllSubcomponents

trait CanAverageParameters extends Module {
  def averaging: Boolean = true

  def useAverage: Boolean
}

trait Perceptron extends Module {
  def learn(obs: Observation, gold: State, penalties: Penalties, param: TrainingParameters) = {
    val map = inferMAP(obs, penalties)
    updateWeights(obs, gold, map, param)
  }
}

trait FullyFactorized extends Composed with HasLogger {
  def inferMAP(observation: Observation, penalties: Penalties): State = {
    val result = new MutableState
    val counting = new Counting(10, count => logger.info("Processed %d instances".format(count)))
    for (instance <- counting(decompose(observation))) result ++= instance.inferMAP(penalties).mapping
    result
  }
  override def nBest(n: Int, observation: Observation, penalties: Penalties): Seq[State] = {
    val result = new ArrayBuffer[MutableState]
    for (i <- 0 until n) result += new MutableState
    for (instance <- decompose(observation)) {
      val localBests = instance.nBest(n, penalties)
      for (i <- 0 until localBests.size) result(i) ++= localBests(i).mapping
    }
    result
  }
}


/*
class RelationMentionFeatureExtractor extends Annotator with HasLogger {
 var killerFeature = false

 val proteinTokens = new HashSet[Token]

 val dictionaries = new ArrayBuffer[Dictionary]

 def isSelfMention(mention: SentenceMention[_]) = {
   mention match {
     case rm: RelationMention => rm.argumentCandidates.size == 1 &&
             rm.argumentCandidates.head.arg.head == rm.head
     case _ => false
   }
 }

 def tokenFeatures(token: Token): Seq[String] = {
   for (featureFunction <- tokenFeatureFunctions) yield featureFunction(token)
 }

 val tokenFeatureFunctions: Seq[Token => String] = {
//    dictionaries.map(dict =>
//      {(token:Token) => if (dict.phrases(token.word.toLowerCase)) "In " + dict.id else "Not in " + dict.id }) ++
   Seq(
     (token:Token) => "",
//      (token:Token) => "word: " + token.word.toLowerCase,
//      (token:Token) => "stem: " + token.stem,
     (token:Token) => "tag: " + token.tag,
     (token:Token) => "type: " + (if (proteinTokens(token)) "PROT" else "Unknown"),
     (token:Token) => "stem/tag: %s/%s".format(token.stem, token.tag)
     )
 }

 //val triggerDictionary = new Dictionary(Conf.get("triggerDict", "dictionaries/chun-event.txt"))

 def triggerFeatures(relMention: RelationMention): HashMap[String, Double] = {
   val sentence = relMention.sentence
   val features = new HashMap[String, Double]
   val prefix = if (isSelfMention(relMention)) "self-" else ""
   if (killerFeature) features(relMention.oid) = 1.0 else {
     for (feature <- tokenFeatures(relMention.head))
       features(prefix + feature) = 1.0

     for (entMention <- sentence.entityMentionCandidates) {
       for (depName <- sentence.getDependencyStructureNames) {
         val paths = sentence.getDependencyStructure(depName).shortestPaths()
         val path = paths.getPath(relMention.head, entMention.head)
         for (pathFeature <- pathFeatures(path)) {
           features(prefix + pathFeature) = 1.0
         }
       }
     }
   }
   features
 }

 def targetString(arg: RelationMentionArgument): String = {
   arg.arg match {
     case em: EntityMention => "PROT"
     case rm: RelationMention => SnowballStemmer.stem(rm.head.word.toLowerCase)
   }
 }

 def pathFeatures(path: DependencyPath): Seq[String] = {
   for (tokenFeatureFunction <- tokenFeatureFunctions; labelled <- Seq(true,false)) yield
     path.toString(labelled, tokenFeatureFunction(_))
 }

 def pathFeatures(arg1: RelationMention, arg2: SentenceMention[_]): Seq[String] = {
   val result = new ArrayBuffer[String]
   val triggerHead = arg1.head
   for (depName <- triggerHead.sentence.getDependencyStructureNames;
        paths: ShortestPaths = triggerHead.sentence.getDependencyStructure(depName).shortestPaths();
        path = paths.getPath(arg1.head, arg2.head)) {
     result ++= pathFeatures(path)
//      for (n <- 1 until 5; ngram <- path.ngrams(n)) {
//        result ++= pathFeatures(ngram).map("%d-gram: %s".format(n,_))
//      }
     result ++= path.tokens.flatMap(tokenFeatures(_)).map("vertex-walk: " + _)
   }
   result
 }

 def argFeatures(arg: RelationMentionArgument): HashMap[String, Double] = {
   val prefix = if (isSelfMention(arg.owner)) "self-" else ""
   val triggerHead = arg.owner.head
   val triggerStem = SnowballStemmer.stem(triggerHead.word.toLowerCase)

   val argHead = arg.arg.head
   val argFeatures = new HashMap[String, Double]
   if (killerFeature) argFeatures(arg.owner.oid + "-" + arg.arg.oid) = 1.0 else {
     val target = targetString(arg)
     argFeatures(prefix + "argHead: " + target) = 1.0
     argFeatures(prefix + "bias") = 1.0

     val pathFeats = pathFeatures(arg.owner, arg.arg).map(prefix + _)
     val pathWithArg = pathFeats.map(_ + target)
     val pathWithTrigger = pathFeats.map(_ + triggerStem)
     for (feature <- pathFeats ++ pathWithArg ++ pathWithTrigger) {
       argFeatures(feature) = 1.0
     }
   }
   argFeatures
 }

 def annotate(doc: Document) = {
   proteinTokens.clear
   for (protein <- doc.allEntityMentionCandidates; token <- protein.span) {
     proteinTokens += token
   }
   val relMentions = doc.allRelationMentionCandidates
   for (relMention: RelationMention <- relMentions) {
     relMention.features = Some(triggerFeatures(relMention))
     for (arg <- relMention.argumentCandidates) {
       arg.features = Some(argFeatures(arg))
     }
   }
   logger.info("Extracted features for doc " + doc.id)
 }
}


*/
