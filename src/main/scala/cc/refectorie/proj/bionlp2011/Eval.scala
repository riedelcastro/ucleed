package cc.refectorie.proj.bionlp2011

import collection.mutable.{HashMap, ArrayBuffer}
import cc.refectorie.proj.factorieie.util.HasLogger
import cc.factorie._
import actors.Actor
import scala.util.Random

/**
 * @author sriedel
 */
object Eval extends HasLogger {
  def defaultCategory(any: Any): String = {
    any match {
      case x: AnyRef => x.getClass.getSimpleName;
      case _ => "All"
    }
  }

  val weightsDst = Conf.get("weightsDst","weights")

  case class Spec[T, M <: Module](module: () => M {type Observation = Iterable[T]},
                                  dataset: Iterable[T] = Iterable.empty,
                                  param: TrainingParameters = new TrainingParameters(1.0, 1.0),
                                  outsideLabel: Any = "None",
                                  category: Any => String = defaultCategory(_),
                                  startTrainHook: M => Unit = (m: M) => {},
                                  startTestHook: M => Unit = (m: M) => {},
                                  evalHook: ResultPerEpoch[T, M] => Unit = (result: ResultPerEpoch[T, M]) => {},
                                  nBestEvalHook: Seq[ResultPerEpoch[T, M]] => Unit = (result: Seq[ResultPerEpoch[T, M]]) => {},
                                  validationEvalHook: ResultPerEpoch[T, M] => Unit = (result: ResultPerEpoch[T, M]) => {},
                                  penalties: Penalties = new Penalties(),
                                  epochs: Int = 5,
                                  gold: State = TrueValueState,
                                  nBestN: Int = 1,
                                  parallelFolds: Boolean = false,
                                  shuffle: Boolean = false)

  class ResultPerCategory(category: String) {
    var fp = 0
    var tp = 0
    var fn = 0

    def totalPredicted = tp + fp

    def totalGold = tp + fn

    def precision = {
      tp.toDouble / totalPredicted
    }

    def recall = {
      tp.toDouble / totalGold
    }

    def f1 = {
      if (precision + recall == 0.0) 0.0 else 2.0 * precision * recall / (precision + recall)
    }

    def measure(gold: Any, guess: Any, outside: Any) {
      if (gold != guess) {
        if (guess != outside) fp += 1
        if (gold != outside) fn += 1
      } else {
        if (guess != outside) tp += 1
      }
    }

    override def toString = ("Result for %s:\n" +
      "TP/FP/FN:  %d/%d/%d\n" +
      "P/R/F1:    %f %f %f").format(category, tp, fp, fn, precision, recall, f1)


  }


  class Result[T, M <: Module] {
    val resultsPerEpoch = new ArrayBuffer[ResultPerEpoch[T, M]]
    val nBestResultsPerEpoch = new ArrayBuffer[Seq[ResultPerEpoch[T, M]]]
    val validationResultsPerEpoch = new ArrayBuffer[ResultPerEpoch[T, M]]
  }

  class ResultPerEpoch[T, M <: Module] extends HashMap[String, ResultPerCategory] {
    var prediction: Option[State] = None
    var dataset: Option[Iterable[T]] = None
    var module: Option[M] = None
    var epoch: Int = 0


    override def toString(): String = {
      val sorted = keys.toList.sorted
      val summary = sorted.map(key => "%5f".format(apply(key).f1)).mkString(" ")
      "Epoch " + epoch + ": " + summary + "\n" + values.mkString("\n")
    }
  }

  def eval[T, M <: Module](guess: State, spec: Spec[T, M], fromDataset: Iterable[T]): ResultPerEpoch[T, M] = {
    val result = new ResultPerEpoch[T, M]
    for (variable <- guess.domain) {
      try {
        val goldValue = spec.gold(variable)
        val guessValue = guess(variable)
        val perCategory = result.getOrElseUpdate(spec.category(variable), new ResultPerCategory(spec.category(variable)))
        perCategory.measure(goldValue, guessValue, spec.outsideLabel)
      } catch {
        case (e:Exception) => {
          logger.warn("Couldn't eval variable " + variable)
        }
      }
    }
    result.prediction = Some(guess)
    result.dataset = Some(fromDataset)
    result
  }

  def evalTrainTest[T, M <: Module](spec: Spec[T, M], train: Iterable[T], test: Iterable[T]): Result[T, M] = {
    val module = spec.module()
    val allResults = new Result[T, M]
    for (epoch <- 0 until spec.epochs) {
      logger.info("Start Training for epoch " + epoch)
      spec.startTrainHook(module)
      module.learn(prepare(train, spec), spec.gold, spec.penalties, spec.param)
      module.storeWeights(weightsDst + "/" + epoch)
      logger.info("Start Testing for epoch " + epoch)
      if (spec.nBestN == 1) {

        spec.startTestHook(module)
        val guess = module.inferMAP(test, new Penalties)
        val result = eval(guess, spec, test)
        result.epoch = epoch
        result.module = Some(module)
        spec.evalHook(result)
        allResults.resultsPerEpoch += result
      } else {
        spec.startTestHook(module)
        val nBestResults = new ArrayBuffer[ResultPerEpoch[T, M]]
        val guesses = module.nBest(spec.nBestN, test, new Penalties)
        for (guess <- guesses) {
          val result = eval(guess, spec, test)
          result.epoch = epoch
          result.module = Some(module)
          nBestResults += result
        }
        spec.nBestEvalHook(nBestResults)
        allResults.nBestResultsPerEpoch += nBestResults
      }
      assert(true)
    }
    allResults
  }

  val random = new Random(0)

  def prepare[T, M <: Module](train: Iterable[T], spec: Spec[T, M]): Iterable[T] = {
    if (spec.shuffle) train.shuffle(random) else train
  }

  def evalTrainTest[T, M <: Module](spec: Spec[T, M], train: Iterable[T], test: Iterable[T], validate: Iterable[T]): Result[T, M] = {
    val module = spec.module()
    val allResults = new Result[T, M]
    for (epoch <- 0 until spec.epochs) {
      def applyToData(test: Iterable[T]): Eval.ResultPerEpoch[T, M] = {
        val guess = module.inferMAP(test, new Penalties)
        val result = eval(guess, spec, test)
        result.epoch = epoch
        result.module = Some(module)
        result
      }
      spec.startTrainHook(module)
      module.learn(prepare(train, spec), spec.gold, spec.penalties, spec.param)
      module.storeWeights(weightsDst + "/" + epoch)

      spec.startTestHook(module)
      if (validate != test) {
        val validationResult = applyToData(validate)
        allResults.validationResultsPerEpoch += validationResult
        spec.validationEvalHook(validationResult)
      }

      val testResult = applyToData(test)
      allResults.resultsPerEpoch += testResult
      spec.evalHook(testResult)

    }
    allResults
  }


  def evalSplit[T, M <: Module](spec: Spec[T, M], ratio: Double): Result[T, M] = {
    val trainSize = (spec.dataset.size * ratio).toInt
    val train = spec.dataset.take(trainSize)
    val test = spec.dataset.drop(trainSize)
    evalTrainTest(spec, train, test)
  }

  def evalFolds[T, M <: Module](spec: Spec[T, M], folds: Int): Result[T, M] = {
    val foldSize = spec.dataset.size / folds
    val modules = for (fold <- 0 until folds) yield spec.module()
    val allResults = new Result[T, M]
    for (epoch <- 0 until spec.epochs) {
      logger.info("Epoch %d".format(epoch))

      case object Task
      case object Result

      class Coordinator(parallel: Boolean) extends Actor {
        val processors = for (fold <- 0 until folds) yield new FoldProcessor(fold)
        val completeGuess = new MutableState

        def act(): Unit = {
          loop {
            react {
              case Task => {
                for (processor <- processors) {
                  processor.start
                  processor ! Task
                  if (!parallel) processor !? Result match {
                    case guess: State => {
                      completeGuess ++= guess.mapping
                    }
                  }
                }
                if (parallel) for (processor <- processors) processor !? Result match {
                  case guess: State => {
                    completeGuess ++= guess.mapping
                  }
                }
              }
              case Result => {
                reply(completeGuess)
                exit
              }
            }

          }
        }

      }
      class FoldProcessor(fold: Int) extends Actor {

        def prepare(data: Iterable[T]) = {
          if (spec.shuffle) data.shuffle else data
        }

        def act(): Unit = {
          var state: State = null
          loop {
            react {
              case Task => {
                logger.info("Fold %d".format(fold))
                val module = modules(fold)
                val testStart = fold * foldSize
                val testEnd = (fold + 1) * foldSize
                val test = spec.dataset.slice(testStart, testEnd)
                val train = prepare(spec.dataset.take(testStart) ++ spec.dataset.drop(testEnd))

                logger.info("Fold %d Learning".format(fold))
                spec.startTrainHook(module)
                module.learn(train, spec.gold, spec.penalties, spec.param)

                logger.info("Fold %d Testing".format(fold))
                spec.startTestHook(module)
                state = module.inferMAP(test, new Penalties)
              }
              case Result => {
                reply(state)
                exit
              }
            }
          }
        }
      }
      val coordinator = new Coordinator(spec.parallelFolds)
      coordinator.start
      coordinator ! Task
      coordinator !? Result
      val result = eval(coordinator.completeGuess, spec, spec.dataset)
      result.epoch = epoch
      result.module = Some(modules.last)
      allResults.resultsPerEpoch += result
      spec.evalHook(result)
    }
    allResults
  }


  def evalOnTrain[T, M <: Module](spec: Spec[T, M]): Result[T, M] = {
    evalTrainTest(spec, spec.dataset, spec.dataset)
  }


}
