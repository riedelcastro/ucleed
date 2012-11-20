package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.util.HasLogger
import cc.factorie.{CategoricalDomain, Variable}
import collection.Set
import collection.mutable.{HashMap, HashSet, ArrayBuffer}

/**
 * Dual decomposition for multiple shared variables based on Komodakis et al., 2007
 *
 * @author sriedel
 */
trait OnDemandDualDecomposed extends DualDecomposed with HasLogger {
  self =>


  def findNewTasks(obs: Observation,
                   result: State,
                   current: Iterable[SubtaskAssignment]): Iterable[SubtaskAssignment]

  override def subgradientDescent(obs: Observation, duals: Duals, iterations: Int): Solution = {

    //execute inference on each instance, get the MAP state
    var subtasks = duals.allInstances

    // added tasks after convergence
    var newTasks: Iterable[SubtaskAssignment] = null

    //mapping from instance to its dual variables/penalties
    val subtask2lambdas = new HashMap[SubtaskAssignment,Penalties]
    subtask2lambdas ++= subtasks.map(instance => instance -> duals.dual(instance).copy)

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

    //result holder
    var result: MutableState = null
    do {
      while (iteration == 0 || iteration < iterations && alpha > stepSizeToTerminate && dualChanged.size > 0) {

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
      result = new MutableState
      //for each variable pick the result of the first instance in the list
      for ((variable, instances) <- var2subtasks) {
        result(variable) = subtask2map(instances.head)(variable)
      }
      result.upperBound = dualObjective
      result.score = this.score(obs, result)

      //test for new subtasks to add
      newTasks = findNewTasks(obs, result, subtasks)
      subtasks = subtasks ++ newTasks
      if (newTasks.size > 0) {
        logger.debug("Found additional %d tasks after %d of %d iterations with dual score %f"
                .format(newTasks.size, iteration, iterations, dualObjective))
        dualChanged ++= newTasks
        for (task <- newTasks) subtask2lambdas(task) = duals.dual(task).copy 
      }

    } while (newTasks.size > 0)

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








