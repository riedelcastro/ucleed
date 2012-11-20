package cc.refectorie.proj.bionlp2011.semisupee

import java.io.File
import cc.refectorie.proj.bionlp2011.{DepStructureAugmenter, Conf, McCloskyParser, LowLevelAnnotation}
import cc.refectorie.proj.factorieie.annotator.{CoreNLPTree2DependenciesConverter, CoreNLPSentenceSplitter, CoreNLPTokenizer}

import com.novus.salat._
import com.novus.salat.global._
import com.mongodb.casbah.Imports._
import collection.mutable.{ArrayBuffer, MultiMap, HashMap}
import cc.refectorie.proj.factorieie.data.{Document, KnowledgeBase}
import cc.refectorie.proj.factorieie.util.HasLogger
import com.mongodb.util.JSON
import org.codehaus.jackson.map.ObjectMapper


/**
 * @author sriedel
 */
case class Doc(source: String, sentences: Seq[DocSentence])
case class Token(word: String, beginChar: Int, endChar: Int, info: TokenInfo)
case class TokenInfo(tag: Option[String] = None, lemma: Option[String] = None)
case class DocSentence(source: String, tokens: Seq[Token], deps: Map[String, Deps])
case class Deps(deps: Seq[Dep])
case class Dep(head: Int, mod: Int, label: String)

trait PersistenceEnv {
  implicit val ctx = new Context {
    val name = Some("TestContext-WhenNecessary")
    override val typeHintStrategy = TypeHintStrategy(when =
      TypeHintFrequency.WhenNecessary, typeHint = TypeHint)
  }

}

object DocReader extends PersistenceEnv {
  val parser = new McCloskyParser("mcclosky",
    Conf.get[File]("rerankparser"), Conf.get[File]("biomodel"), Conf.get("maxSentenceLength", 200))
  val depAugmenter = new DepStructureAugmenter("mcclosky", false, false)
  val depConverter = new CoreNLPTree2DependenciesConverter("mcclosky")

  val kb = new KnowledgeBase()

  def convert(facdoc:Document):Doc = {
    val sentences = for (s <- facdoc.sentences) yield {
      val tokens = for (t <- s.tokens) yield {
        Token(t.word, t.charOffsetBegin, t.charOffsetEnd, TokenInfo())
      }
      val deps = for (e <- s.getDependencyStructure("mcclosky").edges) yield {
        Dep(e.head.indexInSentence, e.mod.indexInSentence, e.label)
      }
      DocSentence(s.source, tokens, Map("mcclosky" -> Deps(deps)))
    }
    Doc(facdoc.source, sentences)
  }

  def parse(facdoc: Document) {
    parser.annotate(facdoc)
    depAugmenter.annotate(facdoc)
    depConverter.annotate(facdoc)
  }

  def readDoc(id: String, text: String) = {
    val facdoc = kb.createDocument(id, text)
    CoreNLPTokenizer.annotate(facdoc)
    CoreNLPSentenceSplitter.annotate(facdoc)
    parse(facdoc)
    convert(facdoc)
  }

  def main(args: Array[String]) {
    val pmdoc = PubmedRetriever.getPubMedDoc("14985354")
    val doc = readDoc("14985354", pmdoc.abstr)
    println(doc)
    val dbo = grater[Doc].asDBObject(doc)
    println(dbo.toString)
  }
}

object DatasetPreparer extends HasLogger with PersistenceEnv {

  case class Span(charOffsetBegin: Int, charOffsetEnd: Int)

  val kb = new KnowledgeBase()

  @throws(classOf[Exception])
  def main(args: Array[String]) {
    val pathwayFiles = Seq("pathways/cd40_pathway.xml")
    val doc2interactions = new HashMap[String, scala.collection.mutable.Set[Interaction]] with MultiMap[String, Interaction]
    //calculate doc -> interaction mapping
    for (f <- pathwayFiles) {
      val pathway = PathwayReader.readPathway(f)
      for (i <- pathway.interactions) {
        for (r <- i.references) {
          val pmId = r.pmid
          doc2interactions.addBinding(pmId, i)
        }
      }
    }
    logger.info("Loaded doc -> interaction mapping")
    //now iterate over documents and interactions
    for ((pmid, interactions) <- doc2interactions.take(1)) {
      logger.info("Processing document %s with %d interactions".format(pmid,interactions.size))
      val pubmedDoc = PubmedRetriever.getPubMedDoc(pmid)
      val proteins = interactions.flatMap(_.components).flatMap(_.molecule.proteinYield)
      //find protein spans in text
      val spans = new ArrayBuffer[Span]
      for (protein <- proteins; name <- protein.name) {
        var current = 0
        while (current >= 0) {
          current = pubmedDoc.abstr.indexOfSlice(name, current)
          if (current >= 0){
            val begin = current
            val end = current + name.length()
            val span = Span(begin,end)
            spans += span
            current += 1
          }
        }
      }
      val facdoc = kb.createDocument(pmid, pubmedDoc.abstr)
      CoreNLPTokenizer.annotate(facdoc)
      CoreNLPSentenceSplitter.annotate(facdoc)
      //retokenize
      for (span <- spans){
        val first = facdoc.tokenAt(span.charOffsetBegin).get
        if (first.charOffsetBegin < span.charOffsetBegin){
          facdoc.splitToken(first, span.charOffsetBegin)
        }
        val last = facdoc.tokenAt(span.charOffsetEnd - 1).get
        if (last.charOffsetEnd > span.charOffsetEnd){
          facdoc.splitToken(last, span.charOffsetEnd)
        }
      }
      //parse
      DocReader.parse(facdoc)
      val doc = DocReader.convert(facdoc)
      val dbo = grater[Doc].asDBObject(doc)
      val back = JSON.parse(dbo.toString)
      println(dbo.toString)
      println(back)
      val mapper = new ObjectMapper()
      mapper.defaultPrettyPrintingWriter().writeValue(System.out,dbo)
    }
  }
}