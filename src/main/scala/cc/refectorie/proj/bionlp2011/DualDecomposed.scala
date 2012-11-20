package cc.refectorie.proj.bionlp2011

import cc.factorie.{CategoricalDomain, Variable}
import collection.mutable.{HashSet, HashMap, ArrayBuffer}
import collection.Set
import cc.refectorie.proj.factorieie.util.{Timer, HasLogger}

/**
 * Dual decomposition for multiple shared variables based on Komodakis et al., 2007
 *
 * @author sriedel
 */
trait DualDecomposed extends Composed with HasLogger {

  /**
   * max number of subgradient steps
   */
  def subgradientSteps: Int

  /**
   * The step size for the given iteration.
   */
  def stepSize(iteration: Int, dualObjectives: Seq[Double]): Double

  /**
   * At what step size do we want to terminate?
   */
  def stepSizeToTerminate: Double = 0.0000001

  /**
   * Convert boolean to double
   */
  def toDouble(boolean: Boolean) = if (boolean) 1.0 else 0.0

  /**
   * Post processing hook
   */
  def postprocess(obs: Observation, original: State): State = original

  /**
   * A solution of one subgradient step
   */
  trait Solution {
    def primals: Primals
    def duals: Duals
  }

  trait Primals {
    def score: Double

    def primal(subtaskAssignment: SubtaskAssignment): State

    def merge: State
  }

  /**
   * Dual solution stats (dual variables, dual scores ...)
   */
  trait Duals {
    def allInstances: Iterable[SubtaskAssignment]

    def score: Double

    def dual(instance: SubtaskAssignment): Penalties
  }

  def inferMAP(obs: Observation, penalties: Penalties) = {
    val duals = new Duals {
      def dual(instance: SubtaskAssignment): Penalties = penalties

      def score: Double = 0.0

      def allInstances: scala.Iterable[SubtaskAssignment] = decompose(obs)
    }
    postprocess(obs, subgradientDescent(obs, duals, subgradientSteps).primals.merge)
  }

  def solveSlaveProblems(subtasks: scala.Iterable[SubtaskAssignment],
                         dualChanged: scala.collection.Set[SubtaskAssignment],
                         subtask2lambdas: scala.collection.Map[SubtaskAssignment, Penalties],
                         previousSolution: Map[SubtaskAssignment, State]): Map[SubtaskAssignment, State] = {
    (for (subtask <- subtasks) yield {
      dualChanged(subtask) match {
        case true => subtask -> subtask.inferMAP(subtask2lambdas(subtask))
        case false => subtask -> previousSolution(subtask)
      }
    }).toMap
  }

  def subgradientDescent(obs: Observation, duals: Duals, iterations: Int): Solution = {

    //execute inference on each instance, get the MAP state
    val subtasks = duals.allInstances

    //mapping from instance to its dual variables/penalties
    val subtask2lambdas = subtasks.map(instance => instance -> duals.dual(instance).copy).toMap

    //current iteration
    var iteration = 0

    //do all components agree in all variables
    var agreement = false

    //mapping from variables to all instances that share the variable
    var var2subtasks: Map[Any, Iterable[SubtaskAssignment]] = null

    //all variables mentioned in the map states of the instances
    var variables: Set[Any] = null

    //mapping from instance to its current MAP state
    var subtask2map: Map[SubtaskAssignment, State] = null

    //dual objective
    var dualObjective = 0.0

    //all dual objectives calculated so far
    val dualObjectives = new ArrayBuffer[Double]

    //for which subtasks have the duals changed
    val dualChanged = new HashSet[SubtaskAssignment]
    dualChanged ++= subtasks

    //stepsize
    var alpha = Double.PositiveInfinity

    while (iteration < iterations && alpha > stepSizeToTerminate && !agreement) {

      //assume agreement and check later whether it holds
      agreement = true

      //current stepsize
      alpha = stepSize(iteration, dualObjectives)

      //ask batch module for observations

      //calls MAP inference for those tasks that have updated duals, remembers results
      subtask2map = solveSlaveProblems(subtasks, dualChanged, subtask2lambdas, subtask2map)

      //      println("-------")
      //      for ((subtask,map) <- subtask2map){
      //        println("Subtask: " + subtask)
      //        println(map)
      //      }
      //subtask2map = subtasks.map(instance => instance -> instance.inferMAP(subtask2lambdas(instance))).toMap

      //the current dual objective. This is the sum of all scores of all submodules
      dualObjective = subtask2map.values.map(_.score).sum

      //remember dual objective
      dualObjectives += dualObjective

      //all variables that appear in the map states
      variables = subtask2map.values.flatMap(_.domain).toSet

      //find all instances sharing the same variable
      var2subtasks = variables.map(shared => shared -> subtasks.filter(subtask2map(_).domain(shared))).toMap

      //clear dual changed state because after the last call to MAP on the subtasks it might have changed
      dualChanged.clear

      //we iterate over all variables and calculate new duals
      for ((variable, subtasks) <- var2subtasks) {
        //count number of instances that share variable
        val sharedCount = subtasks.size
        //iterate over all states of the variable todo: factor out call to domain
        for (value <- variable.asInstanceOf[Variable].domain.asInstanceOf[CategoricalDomain[_]]) {
          //count in how many instances the variable is in that state
          val total = subtasks.filter(subtask2map(_)(variable) == value).size.toDouble
          //now update lambdas for this state for all shared instances
          for (subtask <- subtasks) {
            val instancePenalties = subtask2lambdas(subtask)
            val delta = toDouble(subtask2map(subtask)(variable) == value) - total / sharedCount
            val lambda = instancePenalties.penalty(variable, value) - alpha * delta
            instancePenalties(variable -> value) = lambda
            agreement = agreement && delta == 0
            if (delta != 0) dualChanged += subtask
          }
        }
      }
      //next iteration
      iteration += 1
      logger.trace("Finished iteration %d with step size %f and dual score %f".format(iteration, alpha, dualObjective))
    }
    val result = new MutableState

    //for each variable pick the result of the first instance in the list
    for ((variable, instances) <- var2subtasks) {
      result(variable) = subtask2map(instances.head)(variable)
    }

    result.upperBound = dualObjective
    result.score = this.score(obs, result)
    logger.debug("Finished subgradient after %d of %d iteration with dual score %f".
      format(iteration, iterations, dualObjective))

    new Solution {
      def duals = new Duals {
        def dual(instance: SubtaskAssignment): Penalties = subtask2lambdas(instance)

        def score: Double = dualObjective

        def allInstances: scala.Iterable[SubtaskAssignment] = subtasks
      }

      def primals = new Primals {
        def primal(instance: SubtaskAssignment): State = subtask2map(instance)

        def score: Double = result.score

        def merge: State = result
      }
    }
  }
}

trait HasInitialStepSize extends DualDecomposed {
  def initialStepSize: Double
}

trait HarmonicStepsize extends HasInitialStepSize {
  def stepSize(iteration: Int, dualObjectives: scala.Seq[Double]): Double = {
    initialStepSize / (iteration + 1)
  }
}

/**
 * Based on Rush et al. 2010.
 */
trait DualIncreaseCountStepSize extends HasInitialStepSize {

  private def eps = 0.00001

  def stepSize(iteration: Int, dualObjectives: scala.Seq[Double]): Double = {
    var count = 0
    var i = 0
    while (i < iteration - 1) {
      if (dualObjectives(i + 1) > dualObjectives(i) + eps) count += 1
      i += 1
    }
    initialStepSize * scala.Math.pow(2, -count)
  }
}

trait PrimalHeuristic {
  def pickPrimal(solution: DualDecomposed#Solution): State
}

trait RandomPrimalHeuristic extends PrimalHeuristic {
  def pickPrimal(solution: DualDecomposed#Solution): State = {
    null
  }
}


trait DLPW extends DualDecomposed {
  private val dualMemory = new HashMap[Observation, Duals]

  def stepsWithinDLPW: Int

  def learn(obs: Observation, gold: State, penalties: Penalties, param: TrainingParameters) {
    //if we have seen this instance before, get the previous duals, else create new duals
    //based on incoming penalties
    //todo: it worries me that this method will ignore incoming penalties after the first call for a given observation.
    val duals = dualMemory.getOrElseUpdate(obs, new Duals {
      def dual(instance: SubtaskAssignment): Penalties = penalties

      def score: Double = 0.0

      def allInstances: scala.Iterable[SubtaskAssignment] = decompose(obs)
    })

    //descent on dual
    val solution = subgradientDescent(obs, duals, stepsWithinDLPW)

    //learn subcomponent
    for (instance <- solution.duals.allInstances) {
      instance.learn(gold, solution.duals.dual(instance), param)
    }

    //remember duals for next time
    dualMemory(obs) = solution.duals
  }

  /**
   * DLPW needs to remember dual variables from previous runs to start off where it left. This
   * call will free memory by forgetting all these dual variables.
   */
  def clearDualMemory = dualMemory.clear
}

