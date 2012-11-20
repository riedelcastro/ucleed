package cc.refectorie.proj.bionlp2011

import cc.factorie.{CategoricalDomain, Variable}
import collection.Set
import collection.mutable.{HashMap, HashSet, ArrayBuffer}
import cc.refectorie.proj.factorieie.data.{EntityMention, RelationMentionArgument, RelationMention}
import scala.math._
import cc.refectorie.proj.factorieie.util.{Timer, HasLogger}

/**
 * Dual decomposition for multiple shared variables based on Komodakis et al., 2007.
 * This version supports one-way constraints: x - y <= 0. These imply that if x
 * then y, but not they other way around.
 *
 * @author sriedel
 */
trait DualDecomposedOneway extends DualDecomposed with HasLogger {

  trait HasRealRepresentation {
    def variable: Any
    def value: Any
    def toReal(state: State) = {
      state.real(variable, value)
    }
  }
  case class Producer(subtask: SubtaskAssignment, variable: Any, value: Any, channel: Any) extends HasRealRepresentation
  case class Consumer(subtask: SubtaskAssignment, variable: Any, value: Any, channel: Any) extends HasRealRepresentation

  private val eps = 0.0000001

  var totalCount = 0
  var exactCount = 0
  var totalIterationCount = 0

  def reset {
    totalCount = 0
    exactCount = 0
    totalIterationCount = 0
  }

  //  case class Implies(premise:Any, consequence:Any)
  //
  //  def violated(state:State):Seq[Implies]

  def producers(subtask: SubtaskAssignment, state: State): Seq[Producer]
  def consumers(subtask: SubtaskAssignment, producers: Iterable[Producer], state: State): Seq[Consumer]

  def separation(subtask2map: Map[SubtaskAssignment, State], consumerProducers: HashSet[(DualDecomposedOneway.this.type#Producer, DualDecomposedOneway.this.type#Consumer)]): Unit = {
    //find violated one way lambdas: for each subtask and state, get consumers and producers
    //then find all consumer-producer pairs for the same channel, and for each
    //we have one mu.

    val producers = for ((subtask, state) <- subtask2map;
                         producer <- this.producers(subtask, state)) yield producer
    val consumers = for ((subtask, state) <- subtask2map;
                         consumer <- this.consumers(subtask, producers, state)) yield consumer
    //now created mu variables for violated consumer-producer constraints,
    val channels = producers.map(_.channel).toSet
    val channel2producers = channels.map(channel => channel -> producers.filter(_.channel == channel)).toMap
    val channel2consumers = channels.map(channel => channel -> consumers.filter(_.channel == channel)).toMap
    for (channel <- channels) {
      for (producer <- channel2producers(channel);
           consumer <- channel2consumers(channel)) {
        consumerProducers += producer -> consumer
      }
    }
  }
  override def subgradientDescent(obs: Observation, duals: Duals, iterations: Int): Solution = {

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

    val consumerProducers = new HashSet[(Producer, Consumer)]
    val onewayMus = new HashMap[(Producer, Consumer), Double]

    while (iteration < iterations && alpha > stepSizeToTerminate && !agreement) {

      //assume agreement and check later whether it holds
      agreement = true

      //current stepsize
      alpha = stepSize(iteration, dualObjectives)

      //ask batch module for observations

      //calls MAP inference for those tasks that have updated duals, remembers results
      subtask2map = solveSlaveProblems(subtasks, dualChanged, subtask2lambdas, subtask2map)

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
        if (variable.isInstanceOf[Variable]) for (value <- variable.asInstanceOf[Variable].domain.asInstanceOf[CategoricalDomain[_]]) {
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
      separation(subtask2map, consumerProducers)
      //now update mus
      def var2String(v: Any) = {
        v match {
          case rm: RelationMention#Label => rm.owner.head.indexInSentence.toString
          case (trigger: RelationMention, prot1: EntityMention, prot2: EntityMention) =>
            "%d -> (%d || %d)".format(trigger.head.indexInSentence, prot1.head.indexInSentence, prot2.head.indexInSentence)
          case arg: RelationMentionArgument#Role => arg.owner.owner.head.indexInSentence.toString + "->" + arg.owner.arg.head.indexInSentence.toString
          case _ => v.toString
        }
      }
      //      val debugOut = new ArrayBuffer[String]
      for ((producer, consumer) <- consumerProducers) {
        val oldMu = onewayMus.getOrElse(producer -> consumer, 0.0)
        val producerState = producer.toReal(subtask2map(producer.subtask))
        val consumerState = consumer.toReal(subtask2map(consumer.subtask))
        val subgradient = consumerState - producerState
        val newMu = oldMu - alpha * subgradient
        val projected = if (newMu < 0.0) 0.0 else newMu
        val delta = projected - oldMu
        //        if (delta != 0) {
        //          debugOut +=
        //            "----------\nChannel: " + producer.channel + "\n" +
        //              "Producer: " + producerState + " (" + var2String(producer.variable) + ")" + "\n" +
        //              "Consumer: " + consumerState + " (" + var2String(consumer.variable) + ")" + "\n" +
        //              "oldMu: %f, newMu: %f, projected: %f".format(oldMu, newMu, projected)
        //        }

        if (Math.abs(delta) <= eps) {
          onewayMus(producer -> consumer) = projected
          dualChanged += producer.subtask
          dualChanged += consumer.subtask
          val producerPenalties = subtask2lambdas(producer.subtask)
          val consumerPenalties = subtask2lambdas(consumer.subtask)
          val producerPenalty = producerPenalties.penalty(producer.variable, producer.value)
          val consumerPenalty = consumerPenalties.penalty(consumer.variable, consumer.value)
          producerPenalties(producer.variable -> producer.value) = producerPenalty - delta
          consumerPenalties(consumer.variable -> consumer.value) = consumerPenalty + delta
          agreement = false
        }
      }
      //      println(debugOut.sorted.mkString("\n"))


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

    totalCount += 1
    totalIterationCount += iteration
    if (iteration < iterations) exactCount += 1


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

  def kBest = 2

  override def nBest(n: Int, obs: Observation, penalties: Penalties): Seq[State] = {
    val duals = new Duals {
      def dual(instance: SubtaskAssignment): Penalties = penalties
      def score: Double = 0.0
      def allInstances: scala.Iterable[SubtaskAssignment] = decompose(obs)
    }
    val result = new ArrayBuffer[State]
    val solutions = nbestSubgradientDescent(obs, duals, subgradientSteps)
    for (i <- 0 until solutions.size) {
      result += postprocess(obs, solutions(i).primals.merge)
    }
    result
  }

  def nbestSubgradientDescent(obs: Observation, duals: Duals, iterations: Int): Seq[Solution] = {

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

    val consumerProducers = new HashSet[(Producer, Consumer)]
    val onewayMus = new HashMap[(Producer, Consumer), Double]


    val solutions = new ArrayBuffer[Solution]
    val nbestDuals = new HashMap[Solution, Double] {
      override def default(key: Solution) = 0.0
    }
    val nbestDualsPerInstance = new HashMap[(SubtaskAssignment, Solution), Double] {
      override def default(key: (SubtaskAssignment, Solution)) = 0.0
    }

    var outerIteration = 0
    val outerObjectives = new ArrayBuffer[Double]


    while (solutions.size < kBest && outerIteration < 100) {

      while (iteration < iterations && alpha > stepSizeToTerminate && !agreement) {

        //assume agreement and check later whether it holds
        agreement = true

        //current stepsize
        alpha = stepSize(iteration, dualObjectives)

        //ask batch module for observations

        //calls MAP inference for those tasks that have updated duals, remembers results
        subtask2map = solveSlaveProblems(subtasks, dualChanged, subtask2lambdas, subtask2map)

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
          if (variable.isInstanceOf[Variable]) for (value <- variable.asInstanceOf[Variable].domain.asInstanceOf[CategoricalDomain[_]]) {
            //count in how many instances the variable is in that state
            val total = subtasks.filter(subtask2map(_)(variable) == value).size.toDouble
            //now update lambdas for this state for all shared instances
            for (subtask <- subtasks) {
              val instancePenalties = subtask2lambdas(subtask)
              val delta = toDouble(subtask2map(subtask)(variable) == value) - total / sharedCount
              val lambda = instancePenalties.penalty(variable, value) - alpha * delta
              instancePenalties(variable -> value) = lambda
              agreement = agreement && delta == 0
              if (delta != 0)
                dualChanged += subtask
            }
          }
        }
        separation(subtask2map, consumerProducers)
        //now update mus
        def var2String(v: Any) = {
          v match {
            case rm: RelationMention#Label => rm.owner.head.indexInSentence.toString
            case (trigger: RelationMention, prot1: EntityMention, prot2: EntityMention) =>
              "%d -> (%d || %d)".format(trigger.head.indexInSentence, prot1.head.indexInSentence, prot2.head.indexInSentence)
            case arg: RelationMentionArgument#Role => arg.owner.owner.head.indexInSentence.toString + "->" + arg.owner.arg.head.indexInSentence.toString
            case _ => v.toString
          }
        }
        //      val debugOut = new ArrayBuffer[String]
        for ((producer, consumer) <- consumerProducers) {
          val oldMu = onewayMus.getOrElse(producer -> consumer, 0.0)
          val producerState = producer.toReal(subtask2map(producer.subtask))
          val consumerState = consumer.toReal(subtask2map(consumer.subtask))
          val subgradient = consumerState - producerState
          val newMu = oldMu - alpha * subgradient
          val projected = if (newMu < 0.0) 0.0 else newMu
          val delta = projected - oldMu
          //        if (delta != 0) {
          //          debugOut +=
          //            "----------\nChannel: " + producer.channel + "\n" +
          //              "Producer: " + producerState + " (" + var2String(producer.variable) + ")" + "\n" +
          //              "Consumer: " + consumerState + " (" + var2String(consumer.variable) + ")" + "\n" +
          //              "oldMu: %f, newMu: %f, projected: %f".format(oldMu, newMu, projected)
          //        }

          if (Math.abs(delta) <= eps) {
            onewayMus(producer -> consumer) = projected
            dualChanged += producer.subtask
            dualChanged += consumer.subtask
            val producerPenalties = subtask2lambdas(producer.subtask)
            val consumerPenalties = subtask2lambdas(consumer.subtask)
            val producerPenalty = producerPenalties.penalty(producer.variable, producer.value)
            val consumerPenalty = consumerPenalties.penalty(consumer.variable, consumer.value)
            producerPenalties(producer.variable -> producer.value) = producerPenalty - delta
            consumerPenalties(consumer.variable -> consumer.value) = consumerPenalty + delta
            agreement = false
          }
        }
        //      println(debugOut.sorted.mkString("\n"))


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

      val instance2Map = new HashMap[SubtaskAssignment, State]
      instance2Map ++= subtask2map

      val solution = new Solution {
        def duals = new Duals {
          def dual(instance: SubtaskAssignment): Penalties = subtask2lambdas(instance)

          val score: Double = dualObjective

          def allInstances: scala.Iterable[SubtaskAssignment] = subtasks
        }

        def primals = new Primals {
          def primal(instance: SubtaskAssignment): State = instance2Map(instance)

          val score: Double = result.upperBound

          def merge: State = result
        }
      }
      //calculate gradient for each mu of the constraints that forbid previous solutions
      val nBestEps = 0.001
      var maxMuDelta = 0.0
      val nBestAlpha = nBestStepSize(outerIteration, outerObjectives)
      updateNBestDuals(solution)
      def updateNBestDuals(solution: Solution) {
        for (old <- solutions) {
          for ((instance, map) <- instance2Map) {
            var mismatchScore = 0.0
            for ((variable, instances) <- var2subtasks;
                 if (instances.exists(_ == instance))) {
              //todo: for now assume each module touches all variables.
              val oldState = old.primals.primal(instance)(variable)
              val newState = map(variable)
              //              println("Matching variable %s; old: %s; new %s".format(variable, oldState, newState))
              if (variable.isInstanceOf[Variable]) {
                if (oldState != newState) mismatchScore += 2
              } else {
                if (oldState != newState) mismatchScore += 1
              }
            }
            val gradient = -mismatchScore + 2
            //            println("mismatch/gradient: %d / %f ".format(mismatchScore.toInt, gradient))
            //          println("match/size/gradient: %d / %d / %f ".format(matchScore.toInt, solutionSize, gradient))
            val change = 1.0 * nBestAlpha * gradient //todo: 1.0 -> stepsize
            val oldMu = nbestDualsPerInstance(instance -> old)
            val newMu = oldMu - change
            val projected = if (newMu > eps) 0.0 else newMu
            nbestDualsPerInstance(instance -> old) = projected
            val delta = oldMu - projected
            //            println("oldMu/newMu/projected: %f / %f / %f".format(oldMu, newMu, projected))
            if (abs(delta) > maxMuDelta) maxMuDelta = abs(delta)
            if (abs(delta) > 0) for ((variable, instances) <- var2subtasks;
                                     if (instances.exists(_ == instance))) {
              val penalties = subtask2lambdas(instance)
              val oldState = old.primals.primal(instance)(variable)
              if (variable.isInstanceOf[Variable]) {
                for (value <- variable.asInstanceOf[Variable].domain.asInstanceOf[CategoricalDomain[_]]) {
                  val oldPenalty = penalties(variable -> value)
                  penalties(variable -> value) = if (oldState == value) oldPenalty - delta else oldPenalty + delta
                }
              } else {
                val oldPenalty = penalties(variable -> true)
                penalties(variable -> true) = if (oldState == true) oldPenalty - delta else oldPenalty + delta
              }
              dualChanged += instance
            }
          }
        }
      }
      logger.info("Finished N-Best iteration %d with outer stepsize %f, inner stepsize %f and max Delta %f".format(
        outerIteration, nBestAlpha, alpha, maxMuDelta))
      if (maxMuDelta < nBestEps && solutions.forall(s =>
        solution.primals.merge.mapping != s.primals.merge.mapping)) {
        solutions += solution
        logger.info("Added solution # %d with score %f".format(solutions.size, solution.primals.score))
        updateNBestDuals(solution)
      }
      agreement = false
      iteration = 0
      dualObjectives.clear
      outerObjectives += dualObjective
      outerIteration += 1
    }
    solutions

  }


  def nBestStepSize(iteration: Int, dualObjectives: scala.Seq[Double]): Double = {
    var count = 0
    var i = 0
    while (i < iteration - 1) {
      if (dualObjectives(i + 1) >= dualObjectives(i)) count += 1
      i += 1
    }
    1.0 * scala.Math.pow(2, -count)
  }


}





