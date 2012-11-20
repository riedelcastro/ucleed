package cc.refectorie.proj.bionlp2011

import java.io.File
import cc.refectorie.proj.factorieie.annotator.Annotator
import cc.refectorie.proj.factorieie.data.Document
import io.Source
import cc.refectorie.proj.factorieie.util.HasLogger
;

class GeniaSentenceSplitter(script:File = Conf.get[File]("geniass")) extends Annotator with HasLogger{
  def annotate(doc: Document) = {

    val prefix = doc.id.replaceAll("/", "_")
    val inputFile = File.createTempFile(prefix, ".txt")
    val outputFile = File.createTempFile(prefix, ".ss")

    val ssCmd = "%s %s %s".format(script.getAbsolutePath, inputFile.getAbsolutePath, outputFile.getAbsolutePath)

    //start parser
    val ss = Runtime.getRuntime().exec(ssCmd)
    //read in error messages from parser
    for (line <- Source.fromInputStream(ss.getErrorStream).getLines)
      logger.info("Sentence Splitter error Stream: " + line)

    var min = 0
    var currentTokenIndex = 0
    for (line <- Source.fromFile(outputFile).getLines) {
      val max = min + line.length
      val sentence = doc.createSentence
      while (doc.tokens(currentTokenIndex).charOffsetBegin < max){
        sentence.addToken(doc.tokens(currentTokenIndex))
        currentTokenIndex += 1
      }
      min = max
    }
    logger.info("Sentence split doc " + doc.id)

  }


}
