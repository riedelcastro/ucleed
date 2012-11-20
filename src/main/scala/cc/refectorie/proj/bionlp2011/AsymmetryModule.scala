package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.data.{RelationMentionArgument, RelationMention, Sentence}
import collection.mutable.{ArrayBuffer, HashMap}
import cc.factorie._
import cc.refectorie.proj.factorieie.util.HasLogger

/**
 * @author sriedel
 */
trait AsymmetryModule extends Module with MakeNoUpdates with TrainedByEnclosingModule with HasLogger {
  type Observation = Sentence
  import BioNLPConstants._


  /**
   * Find the MAP state for the shared variables of this module, under the given penalties
   */
  def inferMAP(obs: Observation, penalties: Penalties = new Penalties): State = {
    val result = new MutableState
    //cluster args and their symmetric args
    val pair2arg = new HashMap[(RelationMention,RelationMention),RelationMentionArgument#Role]
    for (event1 <- obs.relationMentionCandidates; arg <- event1.argumentCandidates;
         if (arg.argIsRelationMention && arg.arg.head.indexInDocument < event1.head.indexInDocument);
         event2 = arg.relationMention){
      pair2arg(event1 -> event2) = arg.role
    }
    val argPairs = new ArrayBuffer[(RelationMentionArgument#Role,RelationMentionArgument#Role)]
    for (event1 <- obs.relationMentionCandidates; arg <- event1.argumentCandidates;
         if (arg.argIsRelationMention && arg.arg.head.indexInDocument > event1.head.indexInDocument);
         event2 = arg.relationMention){
      argPairs += arg.role -> pair2arg(event2 -> event1)
    }
    //now pick best arg for each pair
    for ((arg1,arg2) <- argPairs){
      val scores1 = roles.map(r => r -> penalties.penalty(arg1, r)).toMap
      val argmax1 = scores1.maxByDouble(_._2)
      val scores2 = roles.map(r => r -> penalties.penalty(arg2, r)).toMap
      val argmax2 = scores2.maxByDouble(_._2)
      if (argmax1._1 != None && argmax2._1 != None){
        logger.trace("Loop in arguments")
        if (argmax1._2  - scores1(None) > argmax2._2 - scores2(None)){
          result(arg1) = argmax1._1
          result(arg2) = None
          result.score += argmax1._2 + scores2(None)
        } else {
          result(arg1) = None
          result(arg2) = argmax2._1
          result.score += argmax2._2 + scores1(None)

        }
      } else {
        result(arg1) = argmax1._1
        result(arg2) = argmax2._1
        result.score += argmax1._2 + argmax2._2
      }
    }
    result
  }

}