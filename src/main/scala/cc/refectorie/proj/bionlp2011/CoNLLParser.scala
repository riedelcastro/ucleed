package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.annotator.Annotator
import java.io.File
import io.Source
import cc.refectorie.proj.factorieie.util.HasLogger
import cc.refectorie.proj.factorieie.data.{Token, Sentence, Document}

/**
 * @author sriedel
 */
class CoNLLParser(name: String = "conll", parseDir: File = Conf.get[File]("conllDir"),
                  extension: String = ".tok.conll") extends Annotator with HasLogger {

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
    val enjuFile = new File(parseDir, BioNLPUtils.fileName(doc.id) + extension)
    val source = Source.fromFile(enjuFile)
    var sentence = doc.sentences.head
    var depStructure = sentence.createDependencyStructure(name)
    for (lineRaw <- source.getLines(); line = lineRaw.trim) {
      //println(line)
      if (line == "" && sentence.indexInDocument < doc.sentences.size - 1) {
        sentence = doc.sentences(sentence.indexInDocument + 1)
        depStructure = sentence.createDependencyStructure(name)
      } else if (line != "") {
        val split = line.split("\t").map(_.trim)
        val headIndex = split(6).toInt
        val modIndex = split(0).toInt
        val label = split(7)
        val modWord = split(1)
        val head = sentence.tokenOrRoot(headIndex - 1)
        val mod = sentence.tokenOrRoot(modIndex - 1)
        assert(mod.word == modWord, "Mismatch %s/%s in documention %s".format(modWord, mod.word, sentence.document.id))
        depStructure.createEdge(head, mod, label)
      }
    }
    logger.info("Loaded %s parses for doc %s".format(name, doc.id))

    source.close()

  }
}