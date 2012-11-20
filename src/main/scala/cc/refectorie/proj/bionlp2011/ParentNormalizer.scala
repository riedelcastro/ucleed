package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.annotator.Annotator
import cc.refectorie.proj.factorieie.data._
import collection.mutable.HashSet
import cc.factorie._
import cc.refectorie.proj.factorieie.util.HasLogger

/**
 * Changes gold annotation to make sure that each all theme-parents (or cause-parents)
 * of an argument are coordinated.
 * @author sriedel
 */
object ParentNormalizer extends Annotator with HasLogger {
  import BioNLPConstants._

  def isConjoined(arg1: RelationMention, arg2: RelationMention) = {
    val dep: DependencyStructure = arg1.head.sentence.getDependencyStructure("mcclosky")
    val edges = dep.edges(arg1.head, arg2.head) ++ dep.edges(arg2.head, arg1.head)
    edges.exists(edge => edge.label.startsWith("conj"))
  }

  def removeArgument(arg: RelationMentionArgument) {
    //only remove it if it's not yet removed
    if (arg.exists.trueValue) {
      arg.exists.trueValue = false
      arg.role.trueValue = None
      //test owner event whether it still has a theme
      if (!arg.owner.argumentCandidates.exists(arg => arg.exists.trueValue && arg.role.trueValue == Theme)) {
        removeEvent(arg.owner)
      }
    }
  }

  /**
   * Removes an event, and all events that become invalid without this event.
   */
  def removeEvent(event: RelationMention) {
    if (event.exists.trueValue) {
      event.exists.trueValue = false
      event.label.trueValue = None
      for (arg <- event.argumentInCandidates; if (arg.exists.trueValue)) {
        removeArgument(arg)
      }
    }
  }

  def annotate(doc: Document): Unit = {
    Workbench.place(doc.allRelationMentionCandidates ++ doc.allEntityMentionCandidates)
    val toRemove = new HashSet[RelationMentionArgument]
    for (sentence <- doc.sentences) {
      //for each active gold relation mention or protein check all gold parents /contained in rel mentions
      //if there are several gold parents, check whether they are all coordinated.
      //if so, keep structure as is. If not, remove all incoming edges, and make
      //sure corresponding events are removed if they have become inconsistent
      val mentions: Seq[SentenceMention[_]] = sentence.entityMentionCandidates ++
              sentence.relationMentionCandidates.filter(_.exists.trueValue)
      for (mention <- mentions) {
        val incoming = mention.argumentInCandidates.filter(
          a => a.exists.trueValue && a.role.trueValue == Theme)
        if (incoming.size > 1) {
          //pick left most argument, and check if each other argument is coordinated with it
          val leftMost = incoming.maxByInt(arg => -arg.owner.head.indexInDocument)
          val others = incoming.filter(_ != leftMost)
          val allConjoined = others.forall(arg => isConjoined(arg.owner, leftMost.owner))
          //if not, remember to remove them
          if (!allConjoined) toRemove ++= others
        }
      }
    }
    for (arg <- toRemove) removeArgument(arg)
    logger.info("Removed %d arguments".format(toRemove.size))
    Workbench.clean
  }
}