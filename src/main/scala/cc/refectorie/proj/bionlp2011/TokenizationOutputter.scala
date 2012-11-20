package cc.refectorie.proj.bionlp2011

import java.io.{PrintStream, File}
import cc.refectorie.proj.factorieie.util.{Counting, HasLogger}
import cc.refectorie.proj.factorieie.annotator.{CoreNLPTokenizer, CoreNLPSentenceSplitter}

object TokenizationOutputter {
  def main(args: Array[String]) {
    val dir = new File(args(0))
    dir.mkdirs
    for (doc <- RawKB.documents){
      val file = new File(dir,BioNLPUtils.fileName(doc.id) + ".tok")
      val out = new PrintStream(file)
      for (sentence <- doc.sentences){
        out.println(sentence.tokens.map(_.word).mkString(" "))
      }
      out.close
    }
  }

}

object TokenizeAndSentenceSplit extends HasLogger {
  def main(args: Array[String]) {
    val corpusName = args(0)
    val dataDir = Conf.get[File](corpusName)
    annotate(dataDir, corpusName)
  }

  def annotate(dataDir: File, tag: String) {
    //val train = KB.createCorpus("train",Seq("1.txt","2.txt"))

    val retokenizer = new BioNLPEntityMentionRetokenizer(dataDir)
    val loader = new BioNLPLoader(dataDir, tag, maxCount = Conf.get("maxDocs", Int.MaxValue))
    //    val loader = new BioNLPLoader(dataDir, tag, _.filter(_.getName == "2394747.txt"))
    val parser = new McCloskyParser("mcclosky",
      Conf.get[File]("rerankparser"), Conf.get[File]("biomodel"), Conf.get("maxSentenceLength", 200))
    val goldAnnotator = new BioNLPGoldAnnotator(dataDir)
    val counting = new Counting(10, count => logger.info("Processed %d documents".format(count)))
    val splitter = if (Conf.get("splitSemicolon", false))
      CoreNLPSentenceSplitter else new CoreNLPSentenceSplitter("\\.|[!?;]+")

    for (doc <- counting(loader.lazyLoad(RawKB))) {
      CoreNLPTokenizer.annotate(doc)
      TokenizationFix.annotate(doc)
      retokenizer.annotate(doc)
      CoreNLPSentenceSplitter.annotate(doc)
      parser.annotate(doc)
      doc.persist
    }
    println("Test")
  }


}
