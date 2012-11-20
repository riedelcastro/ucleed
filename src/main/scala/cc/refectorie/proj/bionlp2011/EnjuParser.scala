package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.annotator.Annotator
import java.io.File
import io.Source
import cc.refectorie.proj.factorieie.util.HasLogger
import cc.refectorie.proj.factorieie.data.{Token, Sentence, Document}

/**
 * @author sriedel
 */
class EnjuParser(name: String = "enju") extends Annotator with HasLogger {

  def findToken(sentence: Sentence, index: Int, word: String): Token = {
    var i = StrictMath.min(index, sentence.tokens.size - 1)
    var token = sentence.tokenOrRoot(i)
    while (i > -1 && !token.word.contains(word)) {
      i -= 1
      token = sentence.tokenOrRoot(i)
    }
    if (i != index)
      logger.warn("Used token %s as match for %s in doc %s".format(token.word, word, sentence.document.id))
    token
  }

  def annotate(doc: Document) {
    val parseDir = Conf.get[File]("enjuDir")
    val enjuFile = new File(parseDir, BioNLPUtils.fileName(doc.id) + ".tok.enju")
    val source = Source.fromFile(enjuFile)
    var sentence = doc.sentences.head
    var depStructure = sentence.createDependencyStructure(name)
    for (lineRaw <- source.getLines(); line = lineRaw.trim) {
      //println(line)
      if (line == "" && sentence.indexInDocument < doc.sentences.size - 1) {
        sentence = doc.sentences(sentence.indexInDocument + 1)
        depStructure = sentence.createDependencyStructure(name)
      } else if (line != "") {
        try {
          val split = line.split("\t").map(_.trim)
          val headIndex = split(4).toInt
          val modIndex = split(11).toInt
          val headWord = split(0)
          val modWord = split(7)
          val label = split(5) + "-" + split(6)
          val head = findToken(sentence, headIndex, headWord)
          val mod = findToken(sentence, modIndex, modWord)
          //        assert(head.word == split(0),
          //          "In doc %s head %s at %d does not match %s".format(doc.id, head.word, headIndex, split(0)))
          depStructure.createEdge(head, mod, label)
        } catch {
          case e => {
            logger.warn("Problem in file %s with line: %s".format(doc.id, line))
          }
        }
      }
    }
    logger.info("Loaded Enju parses for doc " + doc.id)

    source.close()

  }
}