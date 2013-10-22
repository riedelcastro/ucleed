package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.loader.DocumentLoader
import java.lang.String
import io.Source
import cc.refectorie.proj.factorieie.data._
import cc.refectorie.proj.factorieie.util._
import java.io._
import mongo.MongoDBPersistence
import cc.refectorie.proj.factorieie.annotator._
import collection.mutable.ArrayBuffer

object Conf extends Config(Util.getStreamFromClassPathOrFile(System.getProperty("prop", "props/bionlp2011.prop"))) {

  import scala.collection.JavaConversions._

  val doTask2 = get("doTask2", false)

  override def toString = {
    val sorted = properties.toMap.toList.sortBy(_._1)
    sorted.map({
      case (key, value) => "%-30s %-30s".format(key + ":", value)
    }).mkString("\n")
  }
}

object AnnotatedDB extends MongoDBPersistence(Conf.get[String]("taskId") + "_annotated_bind")

object AnnotatedKB extends KnowledgeBase(AnnotatedDB)

object DB extends MongoDBPersistence(Conf.get[String]("taskId"))

object KB extends KnowledgeBase(AnnotatedDB)

object RawDB extends MongoDBPersistence(Conf.get[String]("taskId") + "_raw")

object RawKB extends KnowledgeBase(RawDB)

/**
 * Hello world!
 *
 */
object App extends HasLogger {

  val useMcClosky = Conf.get("useMcClosky", true)
  val useEnju = Conf.get("useEnju", false)
  val useGDep = Conf.get("useGDep", false)

  def main(args: Array[String]) {
    val doTask2 = Conf.get("doTask2", false)
    val depConverter = new CoreNLPTree2DependenciesConverter("mcclosky")
    val conjChildAugmenter = new ConjChildAugmenter("mcclosky")
    val triggerDict = new Dictionary(Conf.get[String]("triggerDictFile"))
    val triggerGenerator = new DictionaryBasedTriggerGenerator2(triggerDict,
      token => !token.sentence.getDependencyStructureNames.isEmpty)
    val entityGenerator = if (doTask2) {
      val entityDict = new Dictionary(Conf.get[String]("entityDictFile"))
      new DictionaryBasedEntityGenerator2(entityDict,
        token => !token.sentence.getDependencyStructureNames.isEmpty)
    } else null

    val wwDir = new File(Conf.get("wwDir", "ww"))
    wwDir.mkdirs

    RelationMentionFeatureExtractor.dictionaries += new Dictionary("dictionaries/chun-event.txt")
    RelationMentionFeatureExtractor.dictionaries += new Dictionary("dictionaries/chun-manual-dict.txt")

    val uniqueHyphenMcClosky = Conf.get("uniqueHyphenMcClosky", false)
    val uniqueHyphenEnju = Conf.get("uniqueHyphenEnju", false)
    val uniqueHyphenGDep = Conf.get("uniqueHyphenGDep", false)

    val taskID = Conf.get[String]("taskId")


    val depAugmenter = new DepStructureAugmenter("mcclosky", uniqueHyphenMcClosky, false)
    val enjuAugmenter = new DepStructureAugmenter("enju", false, uniqueHyphenEnju)
    val gdepAugmenter = new DepStructureAugmenter("gdep", uniqueHyphenGDep, false)

    for (dataSetId <- args) {


      val dataDir = new File(Conf.get(dataSetId, "data/corpora/development"))
      val mentionLoader = new BioNLPEntityMentionLoader(dataDir)
      val goldAnnotator = new BioNLPGoldAnnotator(dataDir)

      val ww = new PrintStream(new File(wwDir, "%s-%s.ww".format(taskID, dataSetId)))

      val count = new Counting(20, count => logger.info("Processed %d documents".format(count)))

      println(AnnotatedKB.documents.size)

      for (doc <- count(AnnotatedKB.documents.query(Tag(dataSetId)))) {
        depConverter.annotate(doc)
        if (Conf.get("depAugment", false)) {
          if (useMcClosky) depAugmenter.annotate(doc)
          //          if (useEnju) enjuAugmenter.annotate(doc)
          //          if (useGDep) gdepAugmenter.annotate(doc)
        }
        if (Conf.get("conjChildAugment", false)) conjChildAugmenter.annotate(doc)
        SnowballStemmer.annotate(doc)
        mentionLoader.annotate(doc)
        if (Conf.get("depFix", false)) DependencyStructureFixer.annotate(doc)
        triggerGenerator.annotate(doc)
        if (doTask2) entityGenerator.annotate(doc)
        ArgumentCandidateGenerator.annotate(doc)
        goldAnnotator.annotate(doc)
        ww.println(WhatsWrongOutputGenerator.toWhatsWrong(doc, true))
        //RelationMentionFeatureExtractor.annotate(doc)
        doc.persist
        for (mention <- doc.allRelationMentionCandidates ++ doc.allEntityMentionCandidates) mention.persist
      }
      ww.close
    }
  }


}

object ClearRaw {
  def main(args: Array[String]) {
    RawKB.persistence.dropAll
  }
}

object ClearAnnotated {
  def main(args: Array[String]) {
    AnnotatedKB.persistence.dropAll
    if (args.isEmpty)
      AnnotatedDB.copyFrom(RawDB)
    else {
      val dbName = args(0)
      val db = new MongoDBPersistence(dbName)
      AnnotatedDB.copyFrom(db)
    }
  }
}

object LowLevelAnnotation extends HasLogger {
  def main(args: Array[String]) {
    for (corpusName <- args) {
      val dataDir = Conf.get[File](corpusName)
      annotate(dataDir, corpusName)
    }
  }

  val useMcClosky = Conf.get("useMcClosky", true)
  val useEnju = Conf.get("useEnju", false)
  val useGDep = Conf.get("useGDep", false)


  def annotate(dataDir: File, tag: String) {
    //val train = KB.createCorpus("train",Seq("1.txt","2.txt"))

    val retokenizer = new BioNLPEntityMentionRetokenizer(dataDir)
    val loader = new BioNLPLoader(dataDir, tag, maxCount = Conf.get("maxDocs", Int.MaxValue))
    //    val loader = new BioNLPLoader(dataDir, tag, _.filter(_.getName == "2394747.txt"))
    val parser = new McCloskyParser("mcclosky",
      Conf.get[File]("rerankparser"), Conf.get[File]("biomodel"), Conf.get("maxSentenceLength", 200))
    val enju = new EnjuParser("enju")
    val gdep = new CoNLLParser("gdep", Conf.get[File]("gdepDir"), ".tok.gdep")

    val goldAnnotator = new BioNLPGoldAnnotator(dataDir)
    val counting = new Counting(10, count => logger.info("Processed %d documents".format(count)))
    val splitter = if (Conf.get("splitSemicolon", false))
      CoreNLPSentenceSplitter
    else new CoreNLPSentenceSplitter("\\.|[!?;]+")
    val joiner = new SentenceJoiner(dataDir)

    for (doc <- counting(loader.lazyLoad(RawKB))) {
      CoreNLPTokenizer.annotate(doc)
      TokenizationFix.annotate(doc)
      retokenizer.annotate(doc)
      CoreNLPSentenceSplitter.annotate(doc)
      if (Conf.get("sentenceJoin", true)) joiner.annotate(doc)
      if (useMcClosky) parser.annotate(doc)
      if (useEnju) enju.annotate(doc)
      if (useGDep) gdep.annotate(doc)

      doc.persist
    }
    println("Test")
  }

}

trait BioNLPDictionaryCollector extends HasLogger {

  def checkLine(line: String): Boolean

  def collectFromA2File(in: InputStream, dest: Dictionary) {
    for (line <- Source.fromInputStream(in).getLines; if (checkLine(line))) {
      val Array(_, _, trigger) = line.split("\t")
      dest.add(SnowballStemmer.stem(trigger))
      val split = trigger.split(" ")
      if (split.size > 1) {
        dest.add(SnowballStemmer.stem(split.last))
      }
    }
  }

  def collectFromA2Directory(dir: File, dest: Dictionary) {
    for (file <- BioNLPUtil.getFiles(dir, ".a2")) {
      collectFromA2File(new FileInputStream(file), dest)
    }
  }


}

object BioNLPTriggerDictionaryCollector extends BioNLPDictionaryCollector {

  def checkLine(line: String): Boolean = {
    if (line.startsWith("T")) {
      val Array(_, entityType, _) = line.split("\t")
      !entityType.startsWith("Entity")
    } else false
  }

  def main(args: Array[String]) {
    val dictionary = new Dictionary
    collectFromA2Directory(Conf.get[File]("train"), dictionary)
    dictionary.save(new FileOutputStream(Conf.get[File]("triggerDictFile")))
    logger.info("Saved trigger dictionary with %d entries to %s".format(dictionary.original.size, Conf.get[File]("triggerDictFile")))
  }

}

object BioNLPEntityDictionaryCollector extends BioNLPDictionaryCollector {

  def checkLine(line: String): Boolean = {
    if (line.startsWith("T")) {
      val Array(_, entityType, _) = line.split("\t")
      entityType.startsWith("Entity")
    } else false
  }

  def main(args: Array[String]) {
    val dictionary = new Dictionary
    collectFromA2Directory(Conf.get[File]("train"), dictionary)
    dictionary.save(new FileOutputStream(Conf.get[File]("entityDictFile")))
    logger.info("Saved trigger dictionary with %d entries to %s".format(dictionary.original.size, Conf.get[File]("entityDictFile")))
  }

}


class BioNLPLoader(dir: File, tag: String, filter: Seq[File] => Seq[File] = identity(_), maxCount: Int = Int.MaxValue)
  extends DocumentLoader with HasLogger {
  def lazyLoad(kb: KnowledgeBase): Iterator[Document] = {
    val txtFiles = BioNLPUtil.getFiles(dir, ".txt").take(maxCount)
    for (txtFile <- filter(txtFiles).toIterator) yield {
      logger.info("Loading file %s".format(txtFile.getAbsolutePath))
      val source = Source.fromFile(txtFile).getLines.mkString("\n")
      val normalized = source //source.replaceAll("\\.\\.","__")
      val doc = kb.createDocument(txtFile.getAbsolutePath)
      doc.source = normalized
      doc.tags += tag
      //doc.persist
      doc
    }
  }
}

object BioNLPUtil {
  def getFiles(dir: File, postfix: String): Seq[File] = {
    dir.listFiles(new FilenameFilter {
      def accept(dir: File, name: String): Boolean = {
        name.endsWith(postfix)
      }
    })
  }
}

object ClearLearningKB {
  def main(args: Array[String]) {
    DB.dropAll
    DB.copyFrom(AnnotatedDB)
  }
}


object TestCache {
  def main(args: Array[String]) {
    DB.dropAll
    DB.copyFrom(AnnotatedDB)

    val docs = KB.documents.take(4)
    //    val refs = new ArrayBuffer[Any]
    //    refs ++= docs.map(_.reference)
    //
    Workbench.place(docs)

    println(Cache)

    for (doc <- docs) {
      println(doc.oid)
    }

    println(Cache)

    for (doc <- docs) {
      println(doc.oid)
    }


  }

}

object LowLevelAnnotationJSON extends HasLogger {
  def main(args: Array[String]) {
    for (corpusName <- args) {
      val dataDir = Conf.get[File](corpusName)
      annotate(dataDir, corpusName, "/tmp/bionlp-json")
    }
  }

  val useMcClosky = Conf.get("useMcClosky", true)
  val useEnju = Conf.get("useEnju", false)
  val useGDep = Conf.get("useGDep", false)


  def annotate(dataDir: File, tag: String, targetDir: String) {
    //val train = KB.createCorpus("train",Seq("1.txt","2.txt"))

    val retokenizer = new BioNLPEntityMentionRetokenizer(dataDir)
    val depConverter = new CoreNLPTree2DependenciesConverter("mcclosky")
    val triggerDict = new Dictionary(Conf.get[String]("triggerDictFile"))
    val triggerGenerator = new DictionaryBasedTriggerGenerator2(triggerDict,
      token => !token.sentence.getDependencyStructureNames.isEmpty)
    val mentionLoader = new BioNLPEntityMentionLoader(dataDir)


    val loader = new BioNLPLoader(dataDir, tag, maxCount = Conf.get("maxDocs", Int.MaxValue))
    //    val loader = new BioNLPLoader(dataDir, tag, _.filter(_.getName == "2394747.txt"))
    val parser = new McCloskyParser("mcclosky",
      Conf.get[File]("rerankparser"), Conf.get[File]("biomodel"), Conf.get("maxSentenceLength", 200))
    val enju = new EnjuParser("enju")
    val gdep = new CoNLLParser("gdep", Conf.get[File]("gdepDir"), ".tok.gdep")

    val goldAnnotator = new BioNLPGoldAnnotator(dataDir)
    val counting = new Counting(10, count => logger.info("Processed %d documents".format(count)))
    val splitter = if (Conf.get("splitSemicolon", false))
      CoreNLPSentenceSplitter
    else new CoreNLPSentenceSplitter("\\.|[!?;]+")
    val joiner = new SentenceJoiner(dataDir)

    for (doc <- counting(loader.lazyLoad(RawKB))) {
      CoreNLPTokenizer.annotate(doc)
      TokenizationFix.annotate(doc)
      retokenizer.annotate(doc)
      CoreNLPSentenceSplitter.annotate(doc)
      SnowballStemmer.annotate(doc)

      println("Id: " + doc.id)
      val fileName = new File(doc.id).getName
      val dir = new File(targetDir)
      dir.mkdirs()
      val stem = fileName.slice(0, fileName.indexOf("."))
      val json = new File(dir, stem + ".json")
      println(json.getName)
      val out = new PrintStream(json)
      val wwFile = new File(dir, stem + ".ww")
      val ww = new PrintStream(wwFile)

      if (Conf.get("sentenceJoin", true)) joiner.annotate(doc)
      if (useMcClosky) parser.annotate(doc)
      if (useEnju) enju.annotate(doc)
      if (useGDep) gdep.annotate(doc)
      depConverter.annotate(doc)
      mentionLoader.annotate(doc)
      triggerGenerator.annotate(doc)
      ArgumentCandidateGenerator.annotate(doc)
      goldAnnotator.annotate(doc)

      out.println("{")
      val cleanTxt: String = doc.source.replaceAll("\n", " ")
      out.println( """ "txt":"%s",  """.format(cleanTxt))
      out.println( """ "sentences": [""")
      for ((sentence, senIndex) <- doc.sentences.zipWithIndex) {
        if (senIndex > 0) out.println("  ,")

        out.println(" {")
        val deps = sentence.getDependencyStructure("mcclosky")
        out.println( """   "deps": [  """)
        for ((edge, index) <- deps.edges.zipWithIndex) {
          if (index > 0) out.println("  ,")
          out.println("  {")
          out.println( """    "mod":%s,""".format(edge.mod.indexInSentence))
          out.println( """    "head":%s,""".format(edge.head.indexInSentence))
          out.println( """    "label":"%s" """.format(edge.label))
          out.println("  }")
        }
        out.println( """   ],  """)

        out.println( """   "eventCandidates": [  """)
        for ((mention, index) <- sentence.relationMentionCandidates.zipWithIndex) {
          if (index > 0) out.println("  ,")
          out.println("  {")
          out.println( """    "gold":"%s",""".format(mention.label.trueValue))
          out.println( """    "begin":%s,""".format(mention.begin.indexInSentence))
          out.println( """    "end":%s,""".format(mention.end.indexInSentence + 1))
          out.println( """    "arguments":[""")
          for ((arg, argIndex) <- mention.argumentCandidates.zipWithIndex) {
            if (argIndex > 0) out.println("    ,")
            out.println("    {")
            out.println( """      "gold":"%s",""".format(arg.role.trueValue))
            out.println( """      "begin":%s,""".format(arg.arg.begin.indexInSentence))
            out.println( """      "end":%s""".format(arg.arg.end.indexInSentence + 1))
            out.println("    }")
          }
          out.println( """    ]""")
          out.println("  }")
        }
        out.println( """   ],  """)


        out.println( """   "mentions": [  """)
        for ((mention, index) <- sentence.relationMentionCandidates.zipWithIndex) {
          if (index > 0) out.println("  ,")
          out.println("  {")
          out.println( """    "label":"%s",""".format(mention.label.trueValue))
          out.println( """    "begin":%s,""".format(mention.begin.indexInSentence))
          out.println( """    "end":%s""".format(mention.end.indexInSentence + 1))
          out.println("  }")
        }
        out.println( """   ],  """)


        out.println( """ "tokens": [""")
        for (token <- sentence.tokens) {
          if (token.indexInSentence > 0) out.println("  ,")
          out.println("  {")
          out.println( """    "index":%s,""".format(token.indexInSentence))
          out.println( """    "word":"%s",""".format(token.word))
          for (stem <- token.stem) out.println( """    "stem":"%s",""".format(stem))
          out.println( """    "pos":"%s",""".format(token.tag))
          //out.println( """    "ner":%s,""".format(token.ner))
          out.println( """    "begin":%s,""".format(token.charOffsetBegin))
          out.println( """    "end":%s""".format(token.charOffsetEnd))
          out.println("  }")
          println(cleanTxt.slice(token.charOffsetBegin, token.charOffsetEnd))
        }
        out.println( """  ] """)
        out.println(" }")
      }
      out.println( """ ] """)
      out.println("}")

      out.close()
      ww.println(WhatsWrongOutputGenerator.toWhatsWrong(doc, true))

      println(doc)
    }
    println("Test")
  }

}

