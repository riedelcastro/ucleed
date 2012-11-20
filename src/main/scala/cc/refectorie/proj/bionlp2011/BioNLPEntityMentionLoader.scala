package cc.refectorie.proj.bionlp2011

import java.io.File
import cc.refectorie.proj.factorieie.annotator.Annotator
import io.Source
import cc.refectorie.proj.factorieie.util.{Util, HasLogger}
import BioNLPUtils._
import collection.mutable.ArrayBuffer
import cc.refectorie.proj.factorieie.data.{EntityMention, DependencyStructure, Token, Document}

/**
 * Loads BioNLP a1 files and adds entity mentions accordingly.
 *
 * @author sriedel
 */
class BioNLPEntityMentionLoader(dir: File) extends Annotator with HasLogger {
  var skipAbbrevs = Conf.get("skipAbbrev", false)

  def annotate(a1file: File, doc: Document) {
    val added = new ArrayBuffer[EntityMention]
    for (line <- Source.fromFile(a1file).getLines) {
      val Array(id, labelAndOffsets, text) = line.split("\t")
      val Array(label, begin, end) = labelAndOffsets.split(" ")
      for (token <- doc.tokenAt(end.toInt - 1)) {
        val beginInt = if (text.startsWith(" ")) begin.toInt + 1 else begin.toInt
        val tokenAt = doc.tokenAt(beginInt)
        if (tokenAt == None) {
          error("No token can be found for %s at %d in doc %s".format(text, begin.toInt, doc.tokens.map(_.word).mkString(" ")))
        }
        val beginToken = tokenAt.get
        if (beginToken.sentence.getDependencyStructureNames.isEmpty) {
          logger.warn("Sentence has no parse, we skip it")
        } else {
          //find head
          var head = headFinder(beginToken, token)
          if (!skipAbbrevs || !added.lastOption.map(last => last.end.indexInDocument == beginToken.indexInDocument - 2
            && beginToken.prev.word == "(").getOrElse(false)) {
            val mention = head.sentence.createEntityMention(head)
            mention.entityType.set(label)(null)
            mention.entityType.trueValue = label
            mention.exists.trueValue = true
            mention.exists.set(true)(null)
            mention.begin = beginToken
            mention.end = token
            mention.entityKey = Some(id)
            added += mention
          } else {
            logger.info("Skipped protein %s due to abbreviation".format(id))
          }

        }
      }
    }
  }
  def annotate(doc: Document): Unit = {
    val slashIndex = Util.optMinusOne(doc.id.lastIndexOf('/')).getOrElse(0)
    val filename = doc.id.substring(slashIndex)
    val a1file = new File(dir, filename.substring(0, filename.indexOf('.')) + Conf.get("a1Ending",".a1"))
    annotate(a1file, doc)
    logger.info("Added entity mentions from a1 file " + a1file.getAbsolutePath)
  }
}

class BioNLPEntityMentionRetokenizer(dir: File) extends Annotator with HasLogger {



  def annotate(a1file: File, doc: Document) {
    for (line <- Source.fromFile(a1file).getLines) {
      val Array(id, labelAndOffsets, text) = line.split("\t")
      val Array(label, begin, end) = labelAndOffsets.split(" ")
      val beginInt = begin.toInt
      val endInt = end.toInt
      for (token <- doc.tokenAt(endInt - 1)) {
        if (token.charOffsetEnd > endInt) {
          val (t1, t2) = doc.splitToken(token, endInt)
          //doc.splitToken(t2, endInt+1)
        }
      }
    }
  }
  def annotate(doc: Document): Unit = {
    val slashIndex = Util.optMinusOne(doc.id.lastIndexOf('/')).getOrElse(0)
    val filename = doc.id.substring(slashIndex)
    val a1file = new File(dir, filename.substring(0, filename.indexOf('.')) + ".a1")
    annotate(a1file, doc)
    logger.info("Retoknized with respect to entity mentions from a1 file " + a1file.getAbsolutePath)
  }
}

object TokenizationFix extends Annotator with HasLogger {
  def annotate(doc: Document): Unit = {
    //copy tokens since we will make changes to them.
    for (token <- Seq.empty ++ doc.tokens) {
      if (token.indexInDocument == doc.tokens.size - 1 && token.word.size > 1 && token.word.endsWith(".")) {
        doc.splitToken(token, token.charOffsetEnd - 1)
      }
    }
  }
}