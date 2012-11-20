package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.data.{RelationMentionArgument, RelationMention, Sentence}
import collection.mutable.{ArrayBuffer, HashMap}
import cc.factorie._
import cc.refectorie.proj.factorieie.util.HasLogger

/**
 * @author sriedel
 */
trait AntiTransitivityModule extends Module with MakeNoUpdates with TrainedByEnclosingModule with HasLogger {
  type Role = RelationMentionArgument#Role

  //(i,k)(i,j)(j,k)
  type Observation = (Role, Role, Role)
  import BioNLPConstants._

  def unroll(sentence: Sentence): Seq[Observation] = {
    for (event1 <- sentence.relationMentionCandidates;
         arg1 <- event1.argumentCandidates; if (arg1.argIsRelationMention); event2 = arg1.relationMention;
         arg2 <- event2.argumentCandidates;
         arg3 <- event1.argumentCandidates; if (arg2.arg == arg3.arg)) yield {
      (arg3.role, arg2.role, arg1.role)
    }
  }

  /**
   * Find the MAP state for the shared variables of this module, under the given penalties
   */
  def inferMAP(obs: Observation, penalties: Penalties = new Penalties): State = {
    val result = new MutableState
    val scores1 = roles.map(r => r -> penalties.penalty(obs._1, r)).toMap
    val scores2 = roles.map(r => r -> penalties.penalty(obs._2, r)).toMap
    val scores3 = roles.map(r => r -> penalties.penalty(obs._3, r)).toMap

    val argmax1 = scores1.maxByDouble(_._2)
    val argmax2 = scores2.maxByDouble(_._2)
    val argmax3 = scores3.maxByDouble(_._2)

    result(obs._1) = argmax1._1
    result(obs._2) = argmax2._1
    result(obs._3) = argmax3._1
    result.score += argmax1._2 + argmax2._2 + argmax3._2

    if (argmax1._1 != None && argmax2._1 != None && argmax3._1 != None) {
      val weakest = Seq(
        obs._1 -> (argmax1._2 - scores1(None)),
        obs._2 -> (argmax2._2 - scores2(None)),
        obs._3 -> (argmax3._2 - scores3(None))).minByDouble(_._2)
      result(weakest._1) = None
      result.score -= weakest._2
    }
    result
  }

}

trait AntiTransivityFinder extends OnDemandDualDecomposed {
  type Observation = Sentence

  import BioNLPConstants._

  def antiTransitivityModule: Option[AntiTransitivityModule]

  def findNewTasks(sentence: Observation,
                   result: State,
                   current: Iterable[SubtaskAssignment]): Iterable[SubtaskAssignment] = {
    val newTasks = for (module <- antiTransitivityModule) yield {
      val currentSet = current.toSet
      val tasks = for (event1 <- sentence.relationMentionCandidates; if (result(event1.label) != None);
                         arg1 <- event1.argumentCandidates;
                         if (result(arg1.role) != None && arg1.argIsRelationMention); event2 = arg1.relationMention;
                         arg2 <- event2.argumentCandidates; if (result(arg2.role) != None);
                         arg3 <- event1.argumentCandidates; if (result(arg3.role) != None && arg2.arg == arg3.arg);
                         task = Assign(module, (arg3.role, arg2.role, arg1.role));
                         if (!currentSet(task))) yield {
        task
      }
      tasks
    }
    newTasks.getOrElse(Iterable.empty)
  }
}