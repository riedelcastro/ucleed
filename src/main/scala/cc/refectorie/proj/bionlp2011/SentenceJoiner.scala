package cc.refectorie.proj.bionlp2011

import java.io.File
import cc.refectorie.proj.factorieie.annotator.Annotator
import cc.refectorie.proj.factorieie.data.Document
import io.Source
import cc.refectorie.proj.factorieie.util.{Util, HasLogger}


/**
 * Joints sentences that have split proteins
 */
class SentenceJoiner(dir: File) extends Annotator with HasLogger {
  def annotate(a1file: File, doc: Document) {
    var count = 0
    val source = Source.fromFile(a1file)
    for (line <- source.getLines) {
      val Array(id, labelAndOffsets, text) = line.split("\t")
      val Array(label, begin, end) = labelAndOffsets.split(" ")
      val beginInt = begin.toInt
      val endInt = end.toInt
      for (lastToken <- doc.tokenAt(endInt - 1);
           firstToken <- doc.tokenAt(beginInt)) {
        val tokens = doc.tokens.slice(firstToken.indexInDocument, lastToken.indexInDocument + 1)
        for ((first, second) <- tokens.dropRight(1) zip tokens.drop(1)) {
          if (first.sentence != second.sentence) {
            doc.joinSentences(first.sentence, second.sentence)
            count += 1
          }
        }
      }
    }
    source.close
    logger.info("Joined " + count +
      " sentences with respect to entity mentions from a1 file " + a1file.getAbsolutePath)
  }
  def annotate(doc: Document): Unit = {
    val slashIndex = Util.optMinusOne(doc.id.lastIndexOf('/')).getOrElse(0)
    val filename = doc.id.substring(slashIndex)
    val a1file = new File(dir, filename.substring(0, filename.indexOf('.')) + ".a1")
    annotate(a1file, doc)
  }
}


