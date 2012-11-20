package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.data.{EntityMention, RelationMention, Sentence}
import collection.mutable.{HashMap, ArrayBuffer, HashSet}
import BioNLPConstants._
import cc.refectorie.proj.factorieie.util.Util
import java.io.FileOutputStream

/**
 * @author sriedel
 */

trait PairwiseBindingModule extends Module with CanAverageParameters with TrainedByEnclosingModule with MakeNoUpdates {
  self =>
  type Observation = Sentence

  type Pair = Tuple2[EntityMention, EntityMention]
  type TriggerArgTriple = Tuple3[RelationMention, EntityMention, EntityMention]

  val bigramScorer: LocalLinearScorer[Pair] = new ProtPairFeatureScorer {
    def useAverage = self.useAverage
  }

  val eps = 0.000001

  override def loadWeights(prefix: String) {
    val argIs = Util.getStreamFromFileOrClassPath(prefix + "/bi.weights")
    bigramScorer.weights.load(argIs)
  }
  override def storeWeights(prefix: String) {
    val argOut = new FileOutputStream(createDirs(prefix + "/bi.weights"))
    bigramScorer.weights.store(argOut)

  }

  /**
   * Return an aggregation of feature representations for the given observation state. Can return
   * empty map.
   */
  override def features(obs: Observation, state: State): GlobalFeatureVector = {
    val feats = new FeatureVector
    for (prot1 <- obs.entityMentionCandidates;
         prot2 <- obs.entityMentionCandidates;
         if (!prot1.tags(EntityTag) && !prot2.tags(EntityTag));
         if (prot1.head.indexInDocument < prot2.head.indexInDocument)) {
      if (true == state(prot1 -> prot2))
        feats.add(bigramScorer.globalFeatures(prot1 -> prot2, state(prot1 -> prot2)), 1.0)
    }
    new GlobalFeatureVector(Map(bigramScorer -> feats))
  }

  override def featureDelta(obs: Observation, gold: State, guess: State): GlobalFeatureVector = {
    val feats = new FeatureVector
    for (prot1 <- obs.entityMentionCandidates;
         prot2 <- obs.entityMentionCandidates;
         if (!prot1.tags(EntityTag) && !prot2.tags(EntityTag));
         if (prot1.head.indexInDocument < prot2.head.indexInDocument);
         if (gold(prot1 -> prot2) != guess(prot1 -> prot2))) {
      if (true == gold(prot1 -> prot2))
        feats.add(bigramScorer.globalFeatures(prot1 -> prot2, gold(prot1 -> prot2)), 1.0)
      else if (true == guess(prot1 -> prot2))
        feats.add(bigramScorer.globalFeatures(prot1 -> prot2, guess(prot1 -> prot2)), -1.0)
    }
    new GlobalFeatureVector(Map(bigramScorer -> feats))
  }


  /**
   * A weight vector underlying this component. Can be empty.
   */
  override def weights(obs: Observation): GlobalFeatureVector = {
    new GlobalFeatureVector(Map(bigramScorer -> bigramScorer.weights))
  }


  /**
   * An average weight vector underlying this component. Can be empty.
   */
  override def weightsUpdated(obs: Observation) {
    bigramScorer.clearCache
  }
  override def avgWeights(obs: Observation): GlobalFeatureVector = {
    new GlobalFeatureVector(Map(bigramScorer -> bigramScorer.averagingWeights))
  }
  def inferMAP(obs: Observation, penalties: Penalties): State = {
    val result = new MutableRealState(false)
    //first pick binding pairs
    val bound = new HashSet[(EntityMention, EntityMention)]
    val deltaScores = new HashMap[(EntityMention, EntityMention), Double]
    for (prot1 <- obs.entityMentionCandidates;
         prot2 <- obs.entityMentionCandidates;
         if (!prot1.tags(EntityTag) && !prot2.tags(EntityTag));
         if (prot1.head.indexInDocument < prot2.head.indexInDocument)) {
      val pair = prot1 -> prot2
      val trueScore = bigramScorer.score(pair, true) + penalties.penalty(pair, true)
      //val falseScore = bigramScorer.score(pair, false) + penalties.penalty(pair, false)
      result(pair) = trueScore > 0.0
      if (trueScore > 0.0) {
        bound += pair
        result.score += trueScore
      }
      deltaScores(pair) = trueScore
    }
    //now pick event,arg1,arg2 triples based on penalties and whether arg1,arg2 are bound
    //first gather triggerArgPairs with non-zeroPenalties
    for ((variable, value) <- penalties.domain) {
      variable match {
        case t: TriggerArgTriple => bound += t._2 -> t._3
        case _ => {}
      }
    }
    //now pick the best trigger for each arg pair
    for (argPair <- bound) {
      var max = Double.NegativeInfinity
      val argMax = new ArrayBuffer[TriggerArgTriple]
      for (trigger <- obs.relationMentionCandidates;
           if (trigger.argumentCandidates(0).arg.head != trigger.head)) {
        val triggerArgTriple = (trigger, argPair._1, argPair._2)
        val score = penalties.penalty(triggerArgTriple, true)
        //        println("%d -> %d || %d: %f".format(trigger.head.indexInSentence, argPair._1.head.indexInSentence, argPair._2.head.indexInSentence, score))
        if (score > max + eps) {
          max = score
          argMax.clear
          argMax += triggerArgTriple
        } else if (score >= max - eps && score <= max + eps) {
          argMax += triggerArgTriple
        }
      }
      val boundScore = deltaScores(argPair)
      //      println("Argpair %s: %f %f".format(argPair, max, boundScore))
      if (max > 0 || boundScore > -max) {
        for (triple <- argMax) {
          result.reals(triple) = true -> 1.0 / argMax.size
        }
        result.score += max
      } else {
        result(argPair) = false
        result.score -= boundScore
      }
    }
    //    if (!bound.isEmpty) {
    //      println("Bound pairs left: " + bound.filter(pair => true == result(pair)).map({
    //        case(prot1,prot2)=>"%s-%s (%f)".format(prot1.phrase,prot2.phrase,deltaScores(prot1->prot2))}).mkString(","))
    //    }
    result
  }
}

case class Channel(trigger: RelationMention, prot1: EntityMention, prot2: EntityMention) {
  override def toString = "%d -> (%d||%d)".format(trigger.head.indexInSentence, prot1.head.indexInSentence, prot2.head.indexInSentence)
}

object TrueBindingState extends TrueValueState {

  import BioNLPConstants._

  override def apply(variable: Any) = {
    variable match {
      case (prot1: EntityMention, prot2: EntityMention) => {
        val sentence = prot1.head.sentence
        sentence.relationMentionCandidates.exists(relMention => {
          relMention.label.trueValue == Binding && {
            val arg1 = relMention.argumentCandidates.find(_.arg == prot1).get
            val arg2 = relMention.argumentCandidates.find(_.arg == prot2).get
            arg1.role.trueValue == Theme && arg2.role.trueValue == Theme &&
              arg1.relationIDs.exists(id => arg2.relationIDs(id)) &&
              arg1.entityMention.entityKey != arg2.entityMention.entityKey
          }
        })
      }
      case _ => super.apply(variable)
    }
  }
}

trait ProtPairFeatureScorer extends LocalLinearScorer[(EntityMention, EntityMention)] {

  import BioNLPConstants._

  def features(variable: (EntityMention, EntityMention)): scala.Iterable[(Any, Double)] = {
    FeatureCache.protPairFeatures(variable._1, variable._2)
  }

  def domain(variable: (EntityMention, EntityMention)): scala.Iterable[Any] =
    Seq(true, false)

}
