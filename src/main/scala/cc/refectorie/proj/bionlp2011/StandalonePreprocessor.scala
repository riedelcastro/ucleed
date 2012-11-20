/*
 * Created by IntelliJ IDEA.
 * User: riedelcastro
 * Date: 22/02/2011
 * Time: 13:05
 */
package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.annotator.{CoreNLPTokenizer, CoreNLPSentenceSplitter}
import io.Source
import cc.refectorie.proj.factorieie.data.KnowledgeBase
import cc.refectorie.proj.factorieie.util.{HasLogger, Counting}
import java.io.{PrintStream, File}
import collection.mutable.{HashSet, HashMap, ArrayBuffer}

object StandalonePreprocessor extends HasLogger {

  def preprocess(docFile: File, dataDir: File, corpusName: String, outFile: File): Unit = {
    val txt = Source.fromFile(docFile).getLines().mkString("\n")
    val retokenizer = new BioNLPEntityMentionRetokenizer(dataDir)
    val loader = new BioNLPLoader(dataDir, corpusName, maxCount = Conf.get("maxDocs", Int.MaxValue))
    //    val loader = new BioNLPLoader(dataDir, tag, _.filter(_.getName == "2394747.txt"))
    val parser = new McCloskyParser("mcclosky",
      Conf.get[File]("rerankparser"), Conf.get[File]("biomodel"), Conf.get("maxSentenceLength", 200))
    val goldAnnotator = new BioNLPGoldAnnotator(dataDir)
    val counting = new Counting(10, count => logger.info("Processed %d documents".format(count)))
    val splitter = if (Conf.get("splitSemicolon", false))
      CoreNLPSentenceSplitter
    else new CoreNLPSentenceSplitter("\\.|[!?;]+")
    val joiner = new SentenceJoiner(dataDir)
    val kb = new KnowledgeBase()
    val doc = kb.createDocument(docFile.getAbsolutePath, txt)
    CoreNLPTokenizer.annotate(doc)
    TokenizationFix.annotate(doc)
    retokenizer.annotate(doc)
    CoreNLPSentenceSplitter.annotate(doc)
    if (Conf.get("sentenceJoin", true)) joiner.annotate(doc)
    //parser.annotate(doc)
    val out = new PrintStream(outFile)
    for (sentence <- doc.sentences) {
      out.println(sentence.tokens.map(_.word).mkString(" "))
    }
    out.close
  }
  def main(args: Array[String]) {
    val corpusName = args(0)
    val dataDir = Conf.get[File](corpusName)
    val docFile = new File(args(1))
    val outFile = new File(args(2))
    preprocess(docFile, dataDir, corpusName, outFile)
  }

}

object BatchPreprocess {
  def main(args: Array[String]) {
    val corpusName = args(0)
    val dataDir = Conf.get[File](corpusName)
    val txtFiles = BioNLPUtil.getFiles(dataDir, ".txt")
    val outDir = new File(args(1))
    outDir.mkdirs
    for (file <- txtFiles) {
      val outFile = new File(outDir, BioNLPUtils.fileName(file.getAbsolutePath) + ".tok")
      StandalonePreprocessor.preprocess(file, dataDir, corpusName, outFile)

    }
  }
}

object NestedEntityCleaner extends HasLogger {
  def main(args: Array[String]) {
    val corpusName = args(0)
    val dataDir = Conf.get[File](corpusName)
    val txtFiles = BioNLPUtil.getFiles(dataDir, ".txt")
    var violationCount = 0
    var removeCount = 0
    for (file <- txtFiles) {
      val filePrefix = BioNLPUtils.fileName(file.getAbsolutePath)
      val a1File = new File(dataDir, filePrefix + ".a1")
      val a2File = new File(dataDir, filePrefix + ".a2")
      val output = new File(dataDir, filePrefix + ".nn.a1")
      val a1Lines = Source.fromFile(a1File).getLines.toArray
      val id2offset = new HashMap[String, (Int, Int)]
      val rows = new ArrayBuffer[(String, Int, Int)]
      for (line <- a1Lines) {
        val Array(id, labelAndOffsets, text) = line.split("\t")
        val Array(label, begin, end) = labelAndOffsets.split(" ")
        id2offset(id) = begin.toInt -> end.toInt
        rows += ((id, begin.toInt, end.toInt))
      }
      val offsets = id2offset.values.toArray
      val toRemove = new HashSet[String]
      for ((id, begin, end) <- rows) {
        val (begin, end) = id2offset(id)
        val isNested = offsets.exists({
          case (b, e) => (b <= begin && e > end) || (b < begin && e >= end)
        })
        if (isNested) {
          removeCount += 1
          toRemove += id
        }
      }

      val a2Arguments = new HashSet[String]

      //validate
      if (a2File.exists) for (line <- Source.fromFile(a2File).getLines; if (line.startsWith("E"))) {
        val Array(id, typeAndTrigger, rest@_*) = line.split("\\s+")
        for (arg <- rest) {
          val argId = arg.split(":")(1)
          if (toRemove(argId)) violationCount += 1
        }
      }

      //print out
      val out = new PrintStream(output)
      for (line <- a1Lines) {
        val Array(id, labelAndOffsets, text) = line.split("\t")
        if (!toRemove(id)) {
          out.println(line)
        }
      }


    }
    logger.info(("Cleaned a1 files, " +
      "Removed %d entites and found %d violations of the assumption " +
      "that inner proteins can't be arguments").format(removeCount, violationCount))
  }
}