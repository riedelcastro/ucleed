package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.data.{KnowledgeBase, Sentence}
import io.Source
import cc.refectorie.proj.factorieie.data.Tag._
import cc.refectorie.proj.factorieie.annotator.{SnowballStemmer, CoreNLPTree2DependenciesConverter, CoreNLPTokenizer, CoreNLPSentenceSplitter}
import cc.refectorie.proj.factorieie.util.{WhatsWrongOutputGenerator, Dictionary, Counting}
import java.io.{FileOutputStream, PrintStream, File}

/**
 * @author sriedel
 */
class UMassBioEventExtractor extends BioNLPModule with SequentialLearning {
  var testing = false
  val taskID = Conf.get[String]("taskId")
  val evaluator = new BioNLPEvaluator(Conf.get[File]("evalScriptDir"), taskID + "_eval.out")
  var average = Conf.get("average", false)
  val doTask2 = Conf.get("doTask2", false)


  trait UseAverageIfTesting extends CanAverageParameters {
    def useAverage: Boolean = {
      average && testing
    }
  }

  val docModule = new DocModule with SequentialLearning with FullyFactorized with TimedModule {
    val sentenceModule = new SentenceModule4 with Perceptron with DualIncreaseCountStepSize with TimedModule {
      val subgradientSteps: Int = Conf.get("subgradientSteps", 100)

      def averaging: Boolean = true

      def initialStepSize: Double = 0.5

      val asymmetryModule = if (Conf.get("asymmetry", false)) Some(new AsymmetryModule {}) else scala.None

      val antiTransitivityModule = if (Conf.get("antiTransitivity", false)) Some(new AntiTransitivityModule {}) else scala.None

      val relationMentionModule = Conf.get("themeModule", "simple") match {
        case "triggerAware" => new TriggerAwareThemeConsistency with UseAverageIfTesting {}
        case "coordinated" => new CoordinatedThemeConsistency with UseAverageIfTesting {}
        case "triggerAwareCoordinated" => new TriggerAwareCoordinatedThemeConsistency2 with UseAverageIfTesting {}
        case _ => new ThemeConsistency with UseAverageIfTesting {}
      }
      val relationMentionAsTargetModule = if (Conf.get("targetModule", true))
        Some(new ArgumentConsistency with UseAverageIfTesting {})
      else scala.None
      val parentModule = if (Conf.get("useParents", false))
        Some(new EventArgParentModule with UseAverageIfTesting)
      else scala.None
      val siblingModule = if (Conf.get("useSiblings", false))
        Some(new EventArgSiblingModule with UseAverageIfTesting)
      else scala.None

      val bindingModule: Option[PairwiseBindingModule] = {
        if (Conf.get("predictBindingPairs", true))
          Some(new PairwiseBindingModule with UseAverageIfTesting)
        else scala.None
      }

      override def postprocess(sentence: Sentence, original: State): State = {
        if (!Conf.get("postprocess", false)) original else evaluator.postprocess(original, sentence)
      }
    }

  }


  val useMcClosky = Conf.get("useMcClosky", true)
  val useEnju = Conf.get("useEnju", false)
  val useGDep = Conf.get("useGDep", false)

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

  RelationMentionFeatureExtractor.dictionaries += new Dictionary("dictionaries/chun-event.txt")
  RelationMentionFeatureExtractor.dictionaries += new Dictionary("dictionaries/chun-manual-dict.txt")

  val uniqueHyphenMcClosky = Conf.get("uniqueHyphenMcClosky", false)
  val depAugmenter = new DepStructureAugmenter("mcclosky", uniqueHyphenMcClosky, false)

  val mentionLoader = new BioNLPEntityMentionLoader(null)
  val goldAnnotator = new BioNLPGoldAnnotator(null)


  def run(file: File, a1: File, a2:File, dest:File) {
    val kb = new KnowledgeBase
    val txt = Source.fromFile(file).getLines().mkString("\n")
    val docName = file.getName
    val doc = kb.createDocument(docName, txt)
    val retokenizer = new BioNLPEntityMentionRetokenizer(null)
    val parser = new McCloskyParser("mcclosky",
      Conf.get[File]("rerankparser"), Conf.get[File]("biomodel"), Conf.get("maxSentenceLength", 200))
    val enju = new EnjuParser("enju")
//    val gdep = new CoNLLParser("gdep", Conf.get[File]("gdepDir"), ".tok.gdep")
    val joiner = new SentenceJoiner(null)

    //preprocessing
    CoreNLPTokenizer.annotate(doc)
    TokenizationFix.annotate(doc)
    retokenizer.annotate(a1, doc)
    CoreNLPSentenceSplitter.annotate(doc)
    if (Conf.get("sentenceJoin", true)) joiner.annotate(a1,doc)
    if (useMcClosky) parser.annotate(doc)
    if (useEnju) enju.annotate(doc)
//    if (useGDep) gdep.annotate(doc)

    //preparation

    depConverter.annotate(doc)
    if (Conf.get("depAugment", false)) {
      if (useMcClosky) depAugmenter.annotate(doc)
    }
    if (Conf.get("conjChildAugment", false)) conjChildAugmenter.annotate(doc)
    SnowballStemmer.annotate(doc)
    mentionLoader.annotate(a1, doc)
    if (Conf.get("depFix", false)) DependencyStructureFixer.annotate(doc)
    triggerGenerator.annotate(doc)
    if (doTask2) entityGenerator.annotate(doc)
    ArgumentCandidateGenerator.annotate(doc)
    if (a2 != null) goldAnnotator.annotate(a1,a2,doc)

    val state = inferMAP(Seq(doc))
    val events = for (sentence <- doc.sentences;
                      event <- evaluator.stateToBioNLPEvents(state, sentence)) yield event

    val outputStream = new FileOutputStream(dest)
    evaluator.printA2File(outputStream, events)

  }


}

object UMassBioEventExtractor {
  def main(args: Array[String]) {
    val ee = new UMassBioEventExtractor
    val weights = Conf.get("weightsSrc","weights/1")
    ee.loadWeights(weights)

    val txtFile = new File(args(0))
    val a1File = new File(args(1))
    val dst = new File(args(2))
    ee.run(txtFile, a1File, null, dst)
  }
}