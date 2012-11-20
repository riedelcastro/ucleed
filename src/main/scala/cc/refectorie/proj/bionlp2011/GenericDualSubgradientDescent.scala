package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.util.HasLogger
import collection.mutable.{HashMap, ArrayBuffer}

trait Constraint {
  def subgradient(state: State): Double
  def addTo(penalties: Penalties, scale: Double): Unit
}
case class LinearConstraintGE(lhs: Seq[(Any, Any, Double)], rhs: Double) extends Constraint {
  def addTo(penalties: Penalties, scale: Double) = {
    for ((variable, value, coefficient) <- lhs) {
      penalties(variable -> value) = penalties(variable -> value) + coefficient * scale
    }
  }
  def subgradient(state: State) = {
    var score = 0.0
    for ((variable, value, coefficient) <- lhs) {
      if (state(variable) == value) score += coefficient
    }
    val result = score - rhs
    if (result >= 0.0) 0.0 else result
  }
}

trait ConstrainedModule extends Module {

  def constraints(obs: Observation): Seq[Constraint]

}

/**
 * Dual decomposition for multiple shared variables based on Komodakis et al., 2007
 *
 * @author sriedel
 */
trait GenericDualSubgradientDescent extends ConstrainedModule with HasLogger {

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

  abstract override def inferMAP(obs: Observation, penalties: Penalties) = {
    subgradientDescent(obs, penalties)
  }


  def subgradientDescent(obs: Observation, penalties: Penalties): State = {


    //execute inference on each instance, get the MAP state
    val constraints = this.constraints(obs)

    val lambdas = new HashMap[Constraint,Double] {
      override def default(key: Constraint) = 0.0
    }

    var alpha = Double.PositiveInfinity

    var iteration = 0

    var lambdaChanged = true

    val dualObjectives = new ArrayBuffer[Double]

    var y: State = null

    var currentPenalties = penalties.copy

    while (iteration < subgradientSteps && alpha > stepSizeToTerminate && !lambdaChanged) {
      alpha = stepSize(iteration, dualObjectives)
      y = super.inferMAP(obs, currentPenalties)
      dualObjectives += y.score
      lambdaChanged = false
      for (constraint <- constraints) {
        val subgradient = constraint.subgradient(y)
        if (subgradient != 0.0){
          lambdaChanged = true
          val lambda = lambdas(constraint) + alpha * subgradient
          constraint.addTo(currentPenalties,lambda)
          lambdas(constraint) = lambda
        }
      }
      iteration += 1
      logger.trace("Finished iteration %d with step size %f and dual score %f".format(iteration, alpha, y.score))
    }
    logger.debug("Finished subgradient after %d of %d iteration with dual score %f".
      format(iteration, subgradientSteps, y.score))
    y
  }
}


/**
 * Based on Rush et al. 2010.
 */







