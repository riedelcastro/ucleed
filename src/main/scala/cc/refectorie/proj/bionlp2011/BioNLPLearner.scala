/*
 * Created by IntelliJ IDEA.
 * User: riedelcastro
 * Date: 24/02/2011
 * Time: 13:37
 */
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

object BioNLPLearner extends HasLogger {

  import BioNLPConstants._


  def main(args: Array[String]) {

    val doTask2 = Conf.get("doTask2", false)
    val toInclude = args.toSet
    val taskID = Conf.get[String]("taskId")
    val extractor = if (Conf.get("useMultiParseFeatures", false))
      MultiParseRelationMentionFeatureExtractor
    else RelationMentionFeatureExtractor
    val evaluator = new BioNLPEvaluator(Conf.get[File]("evalScriptDir"), taskID + "_eval.out")
    evaluator.groupBindingBySyntax = Conf.get("groupBySyntax", false)
    var average = Conf.get("average", false)
    val doPostProcess = Conf.get("postprocess", false)

    val fpLoss = Conf.get("fpLoss", 1.0)
    val fnLoss = Conf.get("fnLoss", 1.0)
    val otherLoss = Conf.get("otherLoss", 1.0)
    val bindingLossFP = Conf.get("fpBindingLoss", 1.0)
    val bindingLossFN = Conf.get("fnBindingLoss", 1.0)
    val predictBindingPairs = Conf.get("predictBindingPairs", true)


    def globalLoss(sentence: Sentence, gold: State, guess: State): Double = {
      var totalLoss = 0.0
      for (relMention <- sentence.relationMentionCandidates) {
        totalLoss += loss(gold(relMention.label), guess(relMention.label))
        for (arg <- relMention.argumentCandidates) {
          totalLoss += loss(gold(arg.role), guess(arg.role))
        }
      }
      totalLoss
    }

    def globalLossWithBinding(sentence: Sentence, gold: State, guess: State): Double = {
      var totalLoss = 0.0
      for (relMention <- sentence.relationMentionCandidates) {
        totalLoss += loss(gold(relMention.label), guess(relMention.label))
        for (arg <- relMention.argumentCandidates;
             if (arg.argIsRelationMention || doTask2 ||
               !arg.entityMention.tags(EntityTag))) {
          totalLoss += loss(gold(arg.role), guess(arg.role))
        }
      }
      var bindingLoss = 0.0
      if (predictBindingPairs) for (prot1 <- sentence.entityMentionCandidates;
                                    prot2 <- sentence.entityMentionCandidates;
                                    if (!prot1.tags(EntityTag) && !prot2.tags(EntityTag));
                                    if (prot1.head.indexInDocument < prot2.head.indexInDocument)) {

        val goldState = gold(prot1 -> prot2)
        val guessState = guess(prot1 -> prot2)
        if (goldState == true && guessState == false)
          bindingLoss += bindingLossFN
        if (goldState == false && guessState == true)
          bindingLoss += bindingLossFP

      }
      //      println("Binding Loss: " + bindingLoss)
      //      println("Total Loss: " + totalLoss)
      totalLoss + bindingLoss
    }


    def loss(gold: Any, guess: Any): Double = {
      import BioNLPConstants._
      (gold, guess) match {
        case ((triggerGold, argGold), (triggerGuess, argGuess)) => {
          loss(triggerGold, triggerGuess) + loss(argGold, argGuess)
        }
        case (None, _) if (guess != None) => fpLoss
        case (_, None) if (gold != None) => fnLoss
        case (_, _) if (gold != guess) => otherLoss
        case _ => 0.0
      }
    }

    def loss2(gold: Any, guess: Any) = {
      import BioNLPConstants._
      (gold, guess) match {
        case (None, _) if (guess != None) => fpLoss
        case (_, None) if (gold != None) => fnLoss
        case (_, _) if (gold != guess) => otherLoss
        case _ => 0.0
      }
    }

    class MyModule extends BioNLPModule with SequentialLearning {
      var testing = false
      trait UseAverageIfTesting extends CanAverageParameters {
        def useAverage: Boolean = {
          average && testing
        }
      }

      val docModule = new DocModule with SequentialLearning with FullyFactorized with TimedModule {
        val sentenceModule = new SentenceModule4 with Perceptron with DualIncreaseCountStepSize with TimedModule {
          val subgradientSteps: Int = Conf.get("subgradientSteps", 100)

          override def lossFunction: LossFunction[Observation] = new LossFunction[Observation] {
            def loss(obs: Observation, gold: State, guess: State): Double = {
              globalLossWithBinding(obs, gold, guess)
              //globalLoss(obs, gold, guess)
            }
          }

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
            if (predictBindingPairs)
              Some(new PairwiseBindingModule with UseAverageIfTesting)
            else scala.None
          }

          override def postprocess(sentence: Sentence, original: State): State = {
            if (!doPostProcess) original else evaluator.postprocess(original, sentence)
          }
        }

      }
    }


    def module: MyModule = new MyModule

    val forTrain = Conf.get("forTrain", "dev")
    val forTest = Conf.get("forTest", forTrain)
    val forValidation = Conf.get("forValidation", forTest)

    if (Conf.get("dictFeatures", true)) {
      extractor.dictionaries += new Dictionary("dictionaries/chun-event.txt")
      extractor.dictionaries += new Dictionary("dictionaries/chun-manual-dict.txt")
    }


    FeatureCache.useStoredFeatures = Conf.get("useStored", true)
    FeatureCache.caching = Conf.get("cacheFeatures", false)

    val dataMultiplier = Conf.get("dataMultiplier", 1)
    val dataToMultiply =
      if (dataMultiplier != 1)
        Source.fromFile(Conf.get[File]("dataToMultiply")).getLines.map(BioNLPUtils.fileName(_)).toSet
      else
        Set.empty[String]


    def prepare(docs: Iterable[Document]): Iterable[Document] = {
      if (Conf.get("parallelFolds", false)) {
        logger.info("Preloading data")
        val result = for (doc <- docs) yield {
          doc.cacheAllEntityMentionCandidates
          doc.cacheAllRelationMentionCandidates
          doc
        }
        val beforeMultiplication = {
          if (!toInclude.isEmpty)
            result.filter(doc => {
              val fileName = BioNLPUtils.fileName(doc.id)
              //            println(fileName)
              toInclude(fileName)
            }).toList
          else if (Conf.get("onlySentencesWithEntities", false)) result.filter(_.allRelationMentionCandidates.exists({
            rm =>
              rm.argumentCandidates.exists(arg => arg.argIsEntityMention && arg.entityMention.entityType.trueValue == Entity)
          })).toList
          else
            result.toList
        }
        if (dataMultiplier > 1) {
          val toMultiply = beforeMultiplication.filter(doc => dataToMultiply(BioNLPUtils.fileName(doc.id)))
          logger.info("found %d documents to multiply %d times".format(toMultiply.size, dataMultiplier))
          val result = new ArrayBuffer[Document]
          result ++= beforeMultiplication
          for (i <- 0 until dataMultiplier - 1) result ++= toMultiply
          result
        } else if (dataMultiplier == 0) {
          beforeMultiplication.filter(doc => !dataToMultiply(BioNLPUtils.fileName(doc.id)))
        } else beforeMultiplication
      }
      else docs
    }

    val trainDocs = prepare(KB.documents.query(Tag(forTrain)).take(Conf.get("docs", 1000000)))
    val testDocs = if (forTest == forTrain) trainDocs
    else prepare(KB.documents.query(Tag(forTest)).take(Conf.get("docs", 1000000)))
    val validationDocs = if (forTest == forValidation) testDocs
    else prepare(KB.documents.query(Tag(forValidation)).take(Conf.get("docs", 1000000)))

    if (extractor.cutoff > 0) {
      extractor.count(trainDocs)
      logger.info(extractor.stats())
    }


    logger.info("training doc#:   " + trainDocs.size)
    logger.info("test doc#:       " + testDocs.size)
    logger.info("validation doc#: " + validationDocs.size)


    val totals = new ArrayBuffer[BioNLPEvaluator#EvalOutput]

    val labelAccuracyOut = new PrintStream(new FileOutputStream(taskID + "_label_eval.out", true))
    labelAccuracyOut.println(Conf)

    def bionlpEval(result: Eval.ResultPerEpoch[Document, MyModule], target: String) {
      labelAccuracyOut.println(result)
      println(result)
      val evalOutput = evaluator.evaluate(result.dataset.get, result.prediction.get,
        Conf.get[File](target), new File(Conf.get("outDir", new File("out")), taskID + "_" + target + "_epoch_" + result.epoch))
      totals += evalOutput
      println(result.module.get.docModule.timer)
      val sentenceModule = result.module.get.docModule.sentenceModule
      println(sentenceModule.timer)
      println("Ratio of exact solutions: %d / %d: %f".format(
        sentenceModule.exactCount, sentenceModule.totalCount, sentenceModule.exactCount.toDouble / sentenceModule.totalCount))
      println("Average iterations: %d / %d: %f".format(
        sentenceModule.totalIterationCount, sentenceModule.totalCount, sentenceModule.totalIterationCount.toDouble / sentenceModule.totalCount))
    }

    //    Workbench.place(docs)
    //    Workbench.place(docs.flatMap(_.allRelationMentionCandidates))
    //    Workbench.place(docs.flatMap(_.allEntityMentionCandidates))

    //    val goldState = if (Conf.get("learnTransitive", false)) new TransitiveClosure(TrueValueState) else TrueValueState
    val goldState = if (Conf.get("learnTransitive", false)) new TransitiveClosure(TrueValueState) else TrueBindingState

    val evalSpec = Eval.Spec[Document, MyModule](
      module = () => module,
      dataset = trainDocs,
      param = TrainingParameters(Conf.get("learningRate", 1.0), 1.0, loss(_, _)),
      epochs = Conf.get("epochs", 2),
      penalties = if (!Conf.get("lossAugment", false)) new Penalties else new FactorizedLoss(goldState, loss(_, _)),
      evalHook = bionlpEval(_, forTest),
      validationEvalHook = bionlpEval(_, forValidation),
      startTestHook = module => {
        module.docModule.sentenceModule.timer.reset
        module.docModule.sentenceModule.reset
        module.testing = true
      },
      startTrainHook = module => module.testing = false,
      parallelFolds = Conf.get("parallelFolds", false),
      nBestN = Conf.get("nBestN", 1),
      gold = goldState,
      shuffle = Conf.get("shuffle", false))

    logger.info("Start Learning")
    val folds = Conf.get("folds", 3)
    val useDevForTrain = Conf.get("useDevForTrain", false)
    val result = if (forTrain == forTest) {
      if (Conf.get("split", 0.8) < 1.0)
        Eval.evalSplit(evalSpec, Conf.get("split", 0.8))
      else if (folds > 1)
        Eval.evalFolds(evalSpec, folds)
      else
        Eval.evalTrainTest(evalSpec, trainDocs, testDocs)
    } else {
      if (!useDevForTrain)
        Eval.evalTrainTest(evalSpec, trainDocs, testDocs, validationDocs)
      else {
        Eval.evalTrainTest(evalSpec, trainDocs ++ validationDocs, testDocs, testDocs)
      }
    }

    for (evalOutput <- totals) {
      println(evalOutput.table.mkString("\n"))
    }
    for (resultPerEpoch <- result.resultsPerEpoch) {
      println("==============")
      println(resultPerEpoch)
    }


    val wwGold = new PrintStream("/tmp/gold.ww")
    val wwGuess = new PrintStream("/tmp/guess.ww")

    val explain = Conf.get("explain", false)

    //label objects with results
    val solution = result.resultsPerEpoch.last.prediction.head
    val triggerScorer = result.resultsPerEpoch.last.module.get.docModule.sentenceModule.relationMentionModule.triggerScorer
    val argScorer = result.resultsPerEpoch.last.module.get.docModule.sentenceModule.relationMentionModule.argumentScorer
    for (doc <- result.resultsPerEpoch.last.dataset.head) {
      //assign solution to variables of document
      for (relMention: RelationMention <- doc.allRelationMentionCandidates) {
        for (label <- solution.getValue(relMention.label)) {
          relMention.label.set(label)(null)
          if (explain) relMention.label.explanation = Some(
            triggerScorer.explain(relMention.label, relMention.label.value) + "\n===\n" +
              triggerScorer.explain(relMention.label, relMention.label.trueValue))
        }
        if (relMention.label.value != "None") relMention.exists.set(true)(null)
        for (arg <- relMention.argumentCandidates) {
          for (role <- solution.getValue(arg.role)) {
            arg.role.set(role)(null)
            if (explain) arg.role.explanation = Some(
              argScorer.explain(arg.role, arg.role.value) + "\n===\n" +
                argScorer.explain(arg.role, arg.role.trueValue))
          }
          if (arg.role.value != "None") arg.exists.set(true)(null)
        }
      }
      wwGold.println(WhatsWrongOutputGenerator.toWhatsWrong(doc, true))
      wwGuess.println(WhatsWrongOutputGenerator.toWhatsWrong(doc, false))
    }

    //    val sibModule = result.resultsPerEpoch.last.module.get.docModule.sentenceModule.siblingModule
    //    for (parentModule <- result.resultsPerEpoch.last.module.get.docModule.sentenceModule.parentModule) {
    //      println("Parent bigram:")
    //      println(parentModule.bigramScorer.averagingWeights.sortedString)
    //    }
    //    println(sibModule.bigramScorer.averagingWeights.sortedString)

    //    println(sibModule.unigramScorer.averagingWeights.sortedString)

    Workbench.clean
  }

}
