package cc.refectorie.proj.bionlp2011

import collection.mutable.{ListBuffer, HashMap}
import java.util.concurrent.ConcurrentHashMap
import cc.factorie._
import collection.{MapProxy, JavaConversions}
import cc.refectorie.proj.factorieie.data._
import java.io._
import scala.io.Source


/**
 * @author sriedel
 */
class DoubleMap[T] extends HashMap[T, Double] {
  override def default(key: T): Double = 0.0

  def add(key: T, value: Double): Double = {
    val weight = apply(key) + value
    if (weight == 0.0) remove(key)
    else if (value != 0.0) update(key, weight)
    weight
  }

  def add(other: scala.collection.Map[T, Double], scale: Double): Unit = {
    for ((feature, value) <- other) {
      add(feature, value * scale)
    }
  }

  def averagedAdd(key: T, oldCount: Double, count: Double, oldValue: Double, value: Double) = {
    val oldWeight = apply(key)
    val weight = ((oldWeight * oldCount) + (count - oldCount) * oldValue + value) / count
    update(key, weight)
    weight
  }

  def squaredNorm = {
    var result = 0.0
    for ((feature, value) <- this) result += value * value
    result
  }

  def sortedString = {
    this.toSeq.sortBy(_._2).map(pair => "%-70s %-7.4f".format(pair._1, pair._2)).mkString("\n")
  }

  def dot(other: T => Double): Double = {
    var score = 0.0
    for ((feat, value) <- this) score += value * other(feat)
    score
  }

}

object GlobalFeatureVector {
  val empty = new GlobalFeatureVector(Map.empty)
}


class GlobalFeatureVector(init: Map[Any, GenericFeatureVector] = Map.empty) extends HashMap[Any, GenericFeatureVector] {

  this ++= init

  def add(that: GlobalFeatureVector, scale: Double) {
    for ((key, feats) <- that) this(key).add(feats, scale)
  }

  def add(prefix: Any, that: GlobalFeatureVector, scale: Double) {
    for ((key, feats) <- that) this.getOrElseUpdate(prefix -> key, new FeatureVector).add(feats, scale)
  }


  def squaredNorm = {
    values.map(_.squaredNorm).sum
  }

  def dot(that: Any => GenericFeatureVector): Double = {
    this.map({
      case (key, feats) => feats.dot(that(key))
    }).sum
  }

}


trait GenericFeatureVector extends scala.collection.mutable.Map[Any, Double] {
  def add(that: scala.collection.Map[Any, Double], scale: Double): Unit
  def dot(that: Any => Double): Double
  def squaredNorm: Double

  def normalizeString(string:String) = string.replaceAll("\t","-TAB-")

  def store(os:OutputStream) {
    val out = new PrintStream(os)
    for ((key,value) <- this){
      key match {
        case (s1:String,s2:String) =>
          out.println("2s\t%s\t%s\t".format(normalizeString(s1),normalizeString(s2)) + value)
        case (Symbol(s1),Symbol(s2)) =>
          out.println("2\t%s\t%s\t".format(normalizeString(s1),normalizeString(s2)) + value)
        case Symbol(s1) =>
          out.println("1\t%s\t".format(normalizeString(s1)) + value)
        case s =>
          out.println("?\t%s\t".format(normalizeString(s.toString)) + value)
      }
    }
  }

  def load(is:InputStream) {
    val source = Source.fromInputStream(is)
    for (line <- source.getLines){
      val split = line.split("\t")
      split(0) match {
        case "2s" =>
          val s1 = split(1)
          val s2 = split(2)
          val value = split(3).toDouble
          this(s1->s2) = value
        case "2" =>
          val s1 = split(1)
          val s2 = split(2)
          val value = split(3).toDouble
          this(Symbol(s1)->Symbol(s2)) = value
        case "1" =>
          val s1 = split(1)
          val value = split(2).toDouble
          this(Symbol(s1)) = value
        case "?" =>
          val s1 = split(1)
          val value = split(2).toDouble
          this(s1) = value
      }
    }
    source.close()
  }


}

class FeatureVector extends DoubleMap[Any] with GenericFeatureVector
class AveragingFeatureVector extends AveragingDoubleMap[Any] with GenericFeatureVector

/**
 * An averaging double map considers each add operation of an
 * argument x to vector w as w <- w + (w+x)
 */
class AveragingDoubleMap[T] extends DoubleMap[T] {
  var updateCount = 0
  var consistifiedAt = 0
  val average = new DoubleMap[T]

  val oldCounts = new HashMap[T, Int] {
    override def default(key: T): Int = 0
  }

  override def add(x: scala.collection.Map[T, Double], scale: Double) {
    updateCount += 1
    for ((key, value) <- x) {
      val oldValue = apply(key)
      val newValue = oldValue + scale * value
      val oldCount = oldCounts(key)
      val oldAvgValue = average(key)
      val newAverage = (oldAvgValue * oldCount + (updateCount - oldCount - 1) * oldValue + newValue) / updateCount
      average(key) = newAverage
      this(key) = newValue
      oldCounts(key) = updateCount
    }
  }

  def consistify() {
    if (consistifiedAt < updateCount) {
      for (key <- keys) {
        val oldCount = oldCounts(key)
        val oldValue = apply(key)
        val oldAverage = average(key)
        val newAverage = ((oldAverage * oldCount) + (updateCount - oldCount) * oldValue) / updateCount
        average(key) = newAverage
        oldCounts(key) = updateCount
      }
      consistifiedAt = updateCount
    }
  }

}

object AveragingDoubleMap {
  def main(args: Array[String]) {
    val x1 = new DoubleMap[String]
    x1("A") = 1.0
    val x2 = new DoubleMap[String]
    x2("B") = 1.0
    val averaged = new AveragingDoubleMap[String]
    for (i <- 0 until 10) {
      averaged.add(x1, 1.0)
      //averaged.addAveraged(x2, 1.0)
    }
    println(averaged.average)
    averaged.consistify
    //    assert(averaged("A") == 1.0)
    //    assert(averaged("B") == 0.5)
    println(averaged.average)

  }
}


trait Scorer[V] {
  def score(variable: V, value: Any): Double

  def updateWeights(variable: V, gold: Any, guess: Any, param: TrainingParameters): Unit
}

trait NoScore[V] extends Scorer[V] {
  def updateWeights(variable: V, gold: Any, guess: Any, param: TrainingParameters): Unit = {}

  def score(variable: V, value: Any): Double = 0.0
}

trait LocalLinearScorer[V] extends LinearScorer[V] with Module with TrainedByEnclosingModule {
  type Observation = V

  def domain(variable: V): Iterable[Any]

  override def score(obs: V, state: State) = {
    score(obs, state(obs))
  }

  def inferMAP(obs: V, penalties: Penalties) = {
    val state = new MutableState
    state(obs) = domain(obs).maxByDouble(score(obs, _))
    state
  }

  def updateWeights(obs: V, gold: State, guess: State, param: TrainingParameters) = {
    updateWeights(obs, gold(obs), guess(obs), param)
  }
}

case class Example[V](variable: V, gold: Any, guess: Any)
case class Assignment[V](variable: V, value: Any)


trait LinearScorer[V] extends Scorer[V] {
  val weights = new FeatureVector
  val averagingWeights = new AveragingFeatureVector

  private val cache = new HashMap[(V, Any), Double]
  private val cacheFIFO = new ListBuffer[(V, Any)]

  var maxCacheSize = 1000
  var caching = true
  val averaging = true

  def useAverage: Boolean

  def globalFeatures(variable: V, value: Any): FeatureVector = {
    val feats = for ((feature, featureValue) <- features(variable);
                     (labelFeature, labelFeatureValue) <- labelFeatures(value)) yield {
      (feature -> labelFeature) -> featureValue * labelFeatureValue
    }
    val result = new FeatureVector
    result ++= feats
    result
  }


  def cachedScore(variable: V, value: Any): Double = {
    val result = cache.getOrElseUpdate((variable, value), {
      cacheFIFO += variable -> value
      forceScore(variable, value)
    })
    if (cacheFIFO.size > maxCacheSize) {
      val excess = cacheFIFO.take(cacheFIFO.size - maxCacheSize)
      for (toRemove <- excess) {
        cache.remove(toRemove)
      }
      cacheFIFO.remove(0, cacheFIFO.size - maxCacheSize)
    }
    result
  }

  def clearCache = {
    cacheFIFO.clear
    cache.clear
  }

  def features(variable: V): Iterable[(Any, Double)]

  def labelFeatures(label: Any): Iterable[(Any, Double)] = Seq((label, 1.0))


  def score(variable: V, value: Any): Double = {
    if (!caching) forceScore(variable, value) else cachedScore(variable, value)
  }

  def forceScore(variable: V, value: Any): Double = {
    if (useAverage) averagingWeights.consistify
    val weightsToUse = if (useAverage) {
      averagingWeights.average
    } else weights

    var score = 0.0
    for ((feature, featureValue) <- features(variable); (labelFeature, labelFeatureValue) <- labelFeatures(value)) {
      score += featureValue * labelFeatureValue * weightsToUse(feature -> labelFeature)
    }
    score
  }


  def explain(variable: V, value: Any): String = {
    if (useAverage) averagingWeights.consistify
    val weightsToUse = if (useAverage) {
      averagingWeights.average
    } else weights
    val feats = for ((feature, featureValue) <- features(variable);
                     (labelFeature, labelFeatureValue) <- labelFeatures(value)) yield {
      val weight = weightsToUse(feature -> labelFeature) * featureValue * labelFeatureValue
      (weight, feature, labelFeature)
    }
    feats.toSeq.sortBy(_._1).reverse.map(t => {
      val (w, f, l) = t;
      "%-5.4f %-20s %s".format(w, l, f)
    }).mkString("\n")

  }

  def weightsString: String = {
    (for ((featureLabel, weight) <- weights) yield {
      "%-40s %-10.3f".format(featureLabel, weight)
    }).mkString("\n") + "Averaged:\n" +
      (for ((featureLabel, weight) <- averagingWeights.average) yield {
        "%-40s %-10.3f".format(featureLabel, weight)
      }).mkString("\n")
  }

  def batchUpdateWeights(gold: Iterable[Assignment[V]], guess: Iterable[Assignment[V]], param: TrainingParameters) = {
    def lossFunction(gold: Option[Any], guess: Option[Any]) =
      param.loss(gold.getOrElse(BioNLPConstants.None), guess.getOrElse(BioNLPConstants.None))

    val delta = new DoubleMap[(Any, Any)]
    var loss = 0.0
    val goldMap = gold.map(a => a.variable -> a.value).toMap
    val guessMap = guess.map(a => a.variable -> a.value).toMap

    for (example <- gold) {
      for ((feature, value) <- features(example.variable); (labelFeature, labelValue) <- labelFeatures(example.value)) {
        delta.add(feature -> labelFeature, value * labelValue * param.learningRate)
      }
      loss -= score(example.variable, example.value)
      loss += lossFunction(Some(example.value), guessMap.get(example.variable))
      //      loss += (guessMap.get(example.variable) match {
      //        case Some(value) => if (value != example.value) 0.5 else 0.0
      //        case None => 1.0
      //      })

    }
    for (example <- guess) {
      for ((feature, value) <- features(example.variable); (labelFeature, labelValue) <- labelFeatures(example.value)) {
        delta.add(feature -> labelFeature, value * labelValue * (-param.learningRate))
      }
      loss += score(example.variable, example.value)
      loss += lossFunction(goldMap.get(example.variable), Some(example.value))
      //      loss += (goldMap.get(example.variable) match {
      //        case Some(value) => if (value != example.value) 0.5 else 0.0
      //        case None => 1.0
      //      })
    }
    //gold and guess may have different variables, how to calculate score?
    //for shared variable, add 0/1 loss based on values. For every other variable substract 1
    val squaredNorm = delta.squaredNorm
    val learningRate = if (squaredNorm > 0) loss / squaredNorm else 0.0
    weights.add(delta, learningRate * param.learningRate)
    if (averaging) {
      averagingWeights.add(delta, learningRate * param.learningRate)
    }
    if (caching) clearCache

  }

  def batchUpdateWeights(examples: Iterable[Example[V]], param: TrainingParameters) = {
    val delta = new FeatureVector
    var loss = 0.0
    for (example <- examples; if (example.gold != example.guess)) {
      for ((feature, value) <- features(example.variable); (labelFeature, labelValue) <- labelFeatures(example.gold)) {
        delta.add(feature -> labelFeature, value * labelValue * param.learningRate)
      }
      for ((feature, value) <- features(example.variable); (labelFeature, labelValue) <- labelFeatures(example.guess)) {
        delta.add(feature -> labelFeature, value * labelValue * (-param.learningRate))
      }
      loss += score(example.variable, example.guess) - score(example.variable, example.gold) +
        param.loss(example.gold, example.guess)
    }
    val squaredNorm = delta.squaredNorm
    val learningRate = if (squaredNorm > 0) loss / squaredNorm else 0.0
    weights.add(delta, learningRate * param.learningRate)
    if (averaging) {
      averagingWeights.add(delta, learningRate * param.learningRate)
    }
    if (caching) clearCache
  }

  def updateWeights(variable: V, gold: Any, guess: Any, param: TrainingParameters) = {
    batchUpdateWeights(Seq(Example(variable, gold, guess)), param)
  }
}

trait HasFeaturesScorer[V <: HasOwner[HasFeatures]] extends LinearScorer[V] {
  def features(variable: V): Iterable[(Any, Double)] = {
    variable.owner.features.get
  }
}

object FeatureCache {
  var useStoredFeatures = false
  var caching = true
  //val _cache = new HashMap[Any, Iterable[(Any, Double)]]
  val extractor =
    if (Conf.get("useMultiParseFeatures", false))
      MultiParseRelationMentionFeatureExtractor
    else RelationMentionFeatureExtractor


  val _cache = JavaConversions.asMap(new ConcurrentHashMap[Any, Iterable[(Any, Double)]]())

  val _triggerMemo = MemoUtil.concurrentHashMapMemo[RelationMention, Iterable[(Any, Double)]] {
    r => extractor.triggerFeatures(r)
  }

  val _argMemo = MemoUtil.concurrentHashMapMemo[RelationMentionArgument, Iterable[(Any, Double)]] {
    r => extractor.argFeatures(r)
  }

  val _argPairMemo = MemoUtil.concurrentHashMapMemo[(RelationMentionArgument, RelationMentionArgument), Iterable[(Any, Double)]] {
    case (r1, r2) => extractor.argPairFeatures(r1, r2)
  }
  val _protPairMemo = MemoUtil.concurrentHashMapMemo[(EntityMention, EntityMention), Iterable[(Any, Double)]] {
    case (r1, r2) => extractor.protPairFeatures(r1, r2)
  }

  val _argPairParentsMemo = MemoUtil.concurrentHashMapMemo[(RelationMentionArgument, RelationMentionArgument), Iterable[(Any, Double)]] {
    case (r1, r2) => extractor.argPairFeaturesForParents(r1, r2)
  }

  def triggerFeatures(variable: RelationMention): Iterable[(Any, Double)] = {
    if (!useStoredFeatures) {
      //      if (caching) _cache.getOrElseUpdate(variable, RelationMentionFeatureExtractor.triggerFeatures(variable))
      if (caching) _triggerMemo(variable)
      else extractor.triggerFeatures(variable)
    } else variable.features.get

  }

  def argFeatures(variable: RelationMentionArgument): Iterable[(Any, Double)] = {
    if (!useStoredFeatures) {
      //      if (caching) _cache.getOrElseUpdate(variable, RelationMentionFeatureExtractor.argFeatures(variable))
      if (caching) _argMemo(variable)
      else extractor.argFeatures(variable)
    } else variable.features.get
  }

  def argPairFeatures(arg1: RelationMentionArgument, arg2: RelationMentionArgument): Iterable[(Any, Double)] = {
    //    if (caching) _cache.getOrElseUpdate(arg1 -> arg2, RelationMentionFeatureExtractor.argPairFeatures(arg1, arg2))
    if (caching) _argPairMemo(arg1, arg2)
    else extractor.argPairFeatures(arg1, arg2)
  }

  def protPairFeatures(arg1: EntityMention, arg2: EntityMention): Iterable[(Any, Double)] = {
    //    if (caching) _cache.getOrElseUpdate(arg1 -> arg2, RelationMentionFeatureExtractor.argPairFeatures(arg1, arg2))
    if (caching) _protPairMemo(arg1, arg2)
    else extractor.protPairFeatures(arg1, arg2)
  }


  def argPairFeaturesForParents(arg1: RelationMentionArgument, arg2: RelationMentionArgument): Iterable[(Any, Double)] = {
    if (caching) _argPairParentsMemo(arg1, arg2)
    //    if (caching) _cache.getOrElseUpdate(arg1 -> arg2,
    //      RelationMentionFeatureExtractor.argPairFeaturesForParents(arg1, arg2))
    else extractor.argPairFeaturesForParents(arg1, arg2)
  }
}

trait ArgFeatureScorer extends LocalLinearScorer[RelationMentionArgument#Role] {
  def features(variable: RelationMentionArgument#Role): scala.Iterable[(Any, Double)] = {
    FeatureCache.argFeatures(variable.owner)
  }

  def domain(variable: RelationMentionArgument#Role): scala.Iterable[Any] = BioNLPConstants.roles
}

trait ArgPairFeatureScorer extends LocalLinearScorer[(RelationMentionArgument#Role, RelationMentionArgument#Role)] {

  import BioNLPConstants._

  type Arg = RelationMentionArgument#Role

  def features(variable: (Arg, Arg)): scala.Iterable[(Any, Double)] = {
    FeatureCache.argPairFeatures(variable._1.owner, variable._2.owner)
  }

  def domain(variable: (Arg, Arg)): scala.Iterable[Any] =
    Seq(Theme -> Theme, Theme -> Cause, Cause -> Theme, Cause -> Cause)

}

trait ArgPairFeatureScorerForParents extends LocalLinearScorer[(RelationMentionArgument#Role, RelationMentionArgument#Role)] {

  import BioNLPConstants._

  type Arg = RelationMentionArgument#Role

  def features(variable: (Arg, Arg)): scala.Iterable[(Any, Double)] = {
    FeatureCache.argPairFeaturesForParents(variable._1.owner, variable._2.owner)
  }

  def domain(variable: (Arg, Arg)): scala.Iterable[Any] =
    Seq(Theme -> Theme, Theme -> Cause, Cause -> Theme, Cause -> Cause)

}


trait TriggerScorer extends LinearScorer[RelationMention#Label] {
  def features(variable: RelationMention#Label): scala.Iterable[(Any, Double)] = {
    FeatureCache.triggerFeatures(variable.owner)
  }

  override def labelFeatures(label: Any): scala.Iterable[(Any, Double)] = {
    label match {
      case x: String if (x.endsWith("egulation")) => Seq(x -> 1.0, "reg" -> 1.0)
      case x => Seq(x -> 1.0)
    }
  }
}

