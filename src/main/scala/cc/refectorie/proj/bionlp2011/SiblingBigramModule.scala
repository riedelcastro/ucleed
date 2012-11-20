package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.data.{RelationMention, RelationMentionArgument}
import collection.mutable.HashMap
import cc.factorie._
import BioNLPConstants._

/**
 * @author sriedel
 */
trait SiblingBigramModule extends Module {
  type Arg

  def orderedArgs(observation: Observation): Seq[Arg]

  def unigramScorer: LocalLinearScorer[Arg]

  def bigramScorer: LocalLinearScorer[(Arg, Arg)]

  override def weightsUpdated(obs: Observation) {
    unigramScorer.clearCache
    bigramScorer.clearCache
  }


  /**
   * Return an aggregation of feature representations for the given observation state. Can return
   * empty map.
   */
  override def features(obs: Observation, state: State): GlobalFeatureVector = {
    val argFeats = new FeatureVector
    val args = orderedArgs(obs)
    for (arg <- args){
      argFeats.add(unigramScorer.globalFeatures(arg,state(arg)),1.0)
    }
    val nonNone = args.filter(arg => state(arg) != None)
    val biFeats = new FeatureVector
    for ((arg1, arg2) <- nonNone.dropRight(1) zip nonNone.drop(1))
      biFeats.add(bigramScorer.globalFeatures(arg1->arg2,state(arg1)->state(arg2)),1.0)
    new GlobalFeatureVector(Map(unigramScorer->argFeats, bigramScorer -> biFeats))
  }

  override def featureDelta(obs: Observation, gold: State, guess:State): GlobalFeatureVector = {
    val argFeats = new FeatureVector
    val args = orderedArgs(obs)
    for (arg <- args; if (gold(arg) != guess(arg))){
      argFeats.add(unigramScorer.globalFeatures(arg,gold(arg)),1.0)
      argFeats.add(unigramScorer.globalFeatures(arg,guess(arg)),-1.0)
    }
    val goldNones = args.filter(arg => gold(arg) != None)
    val guessNones = args.filter(arg => guess(arg) != None)
    val biFeats = new FeatureVector
    for ((arg1, arg2) <- goldNones.dropRight(1) zip goldNones.drop(1))
      biFeats.add(bigramScorer.globalFeatures(arg1->arg2,gold(arg1)->gold(arg2)),1.0)
    for ((arg1, arg2) <- guessNones.dropRight(1) zip guessNones.drop(1))
      biFeats.add(bigramScorer.globalFeatures(arg1->arg2,guess(arg1)->guess(arg2)),-1.0)
    new GlobalFeatureVector(Map(unigramScorer->argFeats, bigramScorer -> biFeats))
  }


  /**
   * A weight vector underlying this component. Can be empty.
   */
  override def weights(obs: Observation): GlobalFeatureVector = {
    new GlobalFeatureVector(Map(unigramScorer->unigramScorer.weights, bigramScorer -> bigramScorer.weights))    
  }

  /**
   * An average weight vector underlying this component. Can be empty.
   */
  override def avgWeights(obs: Observation): GlobalFeatureVector ={
    new GlobalFeatureVector(Map(unigramScorer->unigramScorer.averagingWeights, bigramScorer -> bigramScorer.averagingWeights))    
  }

  def updateWeights(obs: Observation, gold: State, guess: State, param: TrainingParameters): Unit = {
    val args = orderedArgs(obs)
    unigramScorer.batchUpdateWeights(args.map(arg => Example(arg, gold(arg), guess(arg))), param)
    //bigram scorer
    //positive examples: all C-C, T-T, T-C, C-T transitions in gold
    val positiveNonNone = args.filter(arg => gold(arg) != None)
    val positive = for ((arg1, arg2) <- positiveNonNone.take(positiveNonNone.length - 1) zip positiveNonNone.drop(1)) yield
      Assignment(arg1 -> arg2, gold(arg1) -> gold(arg2))
    //negative examples: the ones in guess
    val negativeNonNone = args.filter(arg => guess(arg) != None)
    val negative = for ((arg1, arg2) <- negativeNonNone.take(negativeNonNone.length - 1) zip negativeNonNone.drop(1)) yield
      Assignment(arg1 -> arg2, guess(arg1) -> guess(arg2))
    bigramScorer.batchUpdateWeights(positive, negative, param)
  }


  /**
   * Find the MAP state for the shared variables of this module, under the given penalties
   */
  def inferMAP(obs: Observation, penalties: Penalties): State = {
    dp(obs, penalties)
  }


  def dp(obs: Observation, penalties: Penalties): State = {
    //need roles in order
    val args = orderedArgs(obs)
    val n = args.size
    if (n == 0) return new MutableState
    val themeMaxMarginals = Array.fill(n)(0.0)
    val causeMaxMarginals = Array.fill(n)(0.0)
    val noneMaxMarginals = Array.fill(n)(0.0)
    val witnesses = new HashMap[(Int, String), (Int, String)]

    def scoreUnigram(arg: Arg, value: String) = {
      penalties(arg -> value) + unigramScorer.score(arg, value)
    }

    trait PairScorer {
      def score(arg1: Arg, arg2: Arg, role1: String, role2: String): Double
    }
    val pairScorer = new PairScorer {
      def score(arg1: Arg, arg2: Arg, role1: String, role2: String) = {
        penalties((arg1 -> arg2) -> (role1 -> role2)) + bigramScorer.score(arg1 -> arg2, role1 -> role2)
      }
    }
    val noneScorer = new PairScorer {
      def score(arg1: Arg, arg2: Arg, role1: String, role2: String) = {
        penalties((arg1 -> arg2) -> (role1 -> role2))
      }
    }

    def setMaxMarginal(i: Int, role: String, dst: Array[Double], scorer: PairScorer): Unit = {
      val s_i = scoreUnigram(args(i), role)
      var max = Double.NegativeInfinity
      var argmax_j = -1
      var argmax_r = role
      for (j <- 0 until i) {
        var none = 0.0
        for (k <- j + 1 until i - 1) none += scoreUnigram(args(k), None)
        val themeScore = themeMaxMarginals(j) + scorer.score(args(j), args(i), Theme, role)
        val causeScore = causeMaxMarginals(j) + scorer.score(args(j), args(i), Cause, role)
        val noneScore = noneMaxMarginals(j)

        var theta_j = Math.max(themeScore, Math.max(causeScore, noneScore)) + none
        if (theta_j > max) {
          argmax_j = j
          argmax_r =
                  if (noneScore >= Math.max(themeScore, causeScore)) None
                  else if (themeScore >= causeScore) Theme else Cause
          max = theta_j
        }
      }
      if (argmax_j != -1) {
        witnesses(i -> role) = argmax_j -> argmax_r
        dst(i) = s_i + max
      } else {
        dst(i) = s_i
      }
    }
    for (i <- 0 until n) {
      setMaxMarginal(i, Theme, themeMaxMarginals, pairScorer)
      setMaxMarginal(i, Cause, causeMaxMarginals, pairScorer)
      setMaxMarginal(i, None, noneMaxMarginals, noneScorer) //this should be doable quicker
    }
    //set all values to None by default
    val result = new MutableState
    for (arg <- args) result(arg) = None

    def backPropagate(index: Int, role: String, state: MutableState): Unit = {
      result(args(index)) = role
      for ((lastIndex, lastRole) <- witnesses.get(index -> role)) {
        backPropagate(lastIndex, lastRole, state)
      }
    }

    if (themeMaxMarginals(n - 1) > Math.max(causeMaxMarginals(n - 1), noneMaxMarginals(n - 1))) {
      backPropagate(n - 1, Theme, result)
      result.score = themeMaxMarginals(n - 1)
    } else if (causeMaxMarginals(n - 1) > Math.max(themeMaxMarginals(n - 1), noneMaxMarginals(n - 1))) {
      backPropagate(n - 1, Cause, result)
      result.score = causeMaxMarginals(n - 1)
    } else {
      backPropagate(n - 1, None, result)
      result.score = noneMaxMarginals(n - 1)
    }

    result

  }

}

trait TriggerAwareSiblingBigramModule extends Module {
  type Arg
  type Trigger

  def orderedArgs(observation: Observation): Seq[Arg]

  def orderedRegArgs(observation: Observation): Seq[Arg]

  def trigger(observation: Observation): Trigger

  def triggerScorer: LocalLinearScorer[Trigger]

  def unigramScorer: LocalLinearScorer[Arg]

  def bigramScorer: LocalLinearScorer[(Arg, Arg)]

  def updateWeights(obs: Observation, gold: State, guess: State, param: TrainingParameters): Unit = {
    val args = orderedArgs(obs)
    //todo: trigger, change role-labels according to trigger
    unigramScorer.batchUpdateWeights(args.map(arg => Example(arg, gold(arg), guess(arg))), param)
    //bigram scorer
    //positive examples: all C-C, T-T, T-C, C-T transitions in gold
    val positiveNonNone = args.filter(arg => gold(arg) != None)
    val positive = for ((arg1, arg2) <- positiveNonNone.take(positiveNonNone.length - 1) zip positiveNonNone.drop(1)) yield
      Assignment(arg1 -> arg2, gold(arg1) -> gold(arg2))
    //negative examples: the ones in guess
    val negativeNonNone = args.filter(arg => guess(arg) != None)
    val negative = for ((arg1, arg2) <- negativeNonNone.take(negativeNonNone.length - 1) zip negativeNonNone.drop(1)) yield
      Assignment(arg1 -> arg2, guess(arg1) -> guess(arg2))
    bigramScorer.batchUpdateWeights(positive, negative, param)
  }




  //roles: Seq(None,Theme,Cause),
  class FullChart(val n: Int, val roles: Seq[String]) {
    val maxMarginals = Array.fill(roles.size)(Array.fill(n)(0.0))
    val maxMarginalsWithTheme = Array.fill(roles.size)(Array.fill(n)(0.0))
    val witnesses = Array.fill(roles.size)(Array.fill(n)((-1) -> (-1)))
    val witnessesWithTheme = Array.fill(roles.size)(Array.fill(n)((-1) -> (-1)))

    def maxMarginal(i: Int, role: Int, withTheme: Boolean) = {
      if (withTheme) maxMarginals(role)(i) else maxMarginalsWithTheme(role)(i)
    }

    def witness(i: Int, role: Int, withTheme: Boolean): (Int, Int) = {
      if (withTheme) witnesses(role)(i) else witnessesWithTheme(role)(i)
    }

    def updateAll(unigram: (Int, String) => Double, bigram: (Int, Int, String, String) => Double) = {
      for (i <- 0 until n; r <- 0 until roles.size) update(i, r, false, unigram, bigram)
      for (i <- 0 until n; r <- 0 until roles.size; if (roles(r) != Theme)) update(i, r, true, unigram, bigram)
    }

    def bestState(dest: MutableState) = {
      null
    }

    def update(i: Int, role: Int, withTheme: Boolean,
               unigram: (Int, String) => Double,
               bigram: (Int, Int, String, String) => Double) {
      var max = Double.NegativeInfinity
      var witness_j = -1
      var witness_r = -1
      val m = if (roles(role) == Theme || !withTheme) maxMarginals else maxMarginalsWithTheme
      for (j <- 0 until i) {
        var noneScore = 0.0
        for (k <- j + 1 until i) noneScore += unigram(k, None)
        for (r <- 0 until roles.size) {
          val score_kr = noneScore + m(r)(j) + (if (r != 0) bigram(j, i, roles(r), roles(role)) else 0.0)
          if (score_kr > max) {
            witness_j = j
            witness_r = r
            max = score_kr
          }
        }
      }
      if (roles(role) == Theme || withTheme) {
        maxMarginalsWithTheme(role)(i) = max
        witnessesWithTheme(role)(i) = witness_j -> witness_r
      }
      if (roles(role) == Theme || !withTheme) {
        maxMarginals(role)(i) = max
        witnesses(role)(i) = witness_j -> witness_r
      }
    }

  }

  def dp2(obs: Observation, penalties: Penalties): State = {
    val args = orderedArgs(obs)
    val regArgs = orderedRegArgs(obs)
    val n = args.size
    val n_reg = regArgs.size
    if (n == 0) return new MutableState

    val trigger = this.trigger(obs)
    val triggerScores = (for (t <- types) yield t->triggerScorer.score(trigger, t)).toMap

    val eventChart = new FullChart(n, Seq(None, Theme))
    val regChart = new FullChart(n_reg, Seq(None, Theme, Cause))

    val bestNone = new MutableState
    bestNone(trigger) = None
    bestNone.score = triggerScores(None)
    for (arg <- args) bestNone(arg) = None
    //todo: should we add scores for None roles here?

    eventChart.updateAll(
      (index, label) => unigramScorer.score(args(index), label),
      (index1, index2, label1, label2) => bigramScorer.score(args(index1) -> args(index2), label1 -> label2))

    val bestNonReg = new MutableState
    eventChart.bestState(bestNonReg)
    val nonReg2Score = triggerScores.filter(t => t._1 != None && !t._1.endsWith("egulation")).maxByDouble(_._2)
    bestNonReg(trigger) = nonReg2Score._1
    bestNonReg.score += nonReg2Score._2

    regChart.updateAll(
      (index, label) => unigramScorer.score(regArgs(index), "reg" -> label),
      (index1, index2, label1, label2) => bigramScorer.score(regArgs(index1) -> regArgs(index2), "reg" -> label1 -> label2))
    val bestReg = new MutableState
    regChart.bestState(bestNonReg)
    val reg2Score = triggerScores.filter(_._1.endsWith("egulation")).maxByDouble(_._2)
    bestReg(trigger) = reg2Score._1
    bestReg.score += reg2Score._2

    Seq(bestNone,bestNonReg,bestReg).maxByDouble(_.score)
  }


  /**
   * Find the MAP state for the shared variables of this module, under the given penalties
   */
  def inferMAP(obs: Observation, penalties: Penalties): State = {
    null //dp(obs, penalties)
  }


  def dp(obs: Observation, penalties: Penalties,
         unigramScorer: LocalLinearScorer[Arg],
         bigramScorer: LocalLinearScorer[(Arg, Arg)],
         allowCause: Boolean): State = {
    //need roles in order
    val args = orderedArgs(obs)
    val n = args.size
    if (n == 0) return new MutableState
    val themeMaxMarginals = Array.fill(n)(0.0)
    val causeMaxMarginals = Array.fill(n)(0.0)
    val noneMaxMarginals = Array.fill(n)(0.0)
    val witnesses = new HashMap[(Int, String), (Int, String)]

    def scoreUnigram(arg: Arg, value: String) = {
      penalties(arg -> value) + unigramScorer.score(arg, value)
    }

    trait PairScorer {
      def score(arg1: Arg, arg2: Arg, role1: String, role2: String): Double
    }
    val pairScorer = new PairScorer {
      def score(arg1: Arg, arg2: Arg, role1: String, role2: String) = {
        penalties((arg1 -> arg2) -> (role1 -> role2)) + bigramScorer.score(arg1 -> arg2, role1 -> role2)
      }
    }
    val noneScorer = new PairScorer {
      def score(arg1: Arg, arg2: Arg, role1: String, role2: String) = {
        penalties((arg1 -> arg2) -> (role1 -> role2))
      }
    }

    def setMaxMarginal(i: Int, role: String, dst: Array[Double], scorer: PairScorer): Unit = {
      val s_i = scoreUnigram(args(i), role)
      var max = Double.NegativeInfinity
      var argmax_j = -1
      var argmax_r = role
      for (j <- 0 until i) {
        var none = 0.0
        for (k <- j + 1 until i - 1) none += scoreUnigram(args(k), None)
        val themeScore = themeMaxMarginals(j) + scorer.score(args(j), args(i), Theme, role)
        val causeScore = if (allowCause)
          causeMaxMarginals(j) + scorer.score(args(j), args(i), Cause, role) else Double.NegativeInfinity
        val noneScore = noneMaxMarginals(j)

        var theta_j = Math.max(themeScore, Math.max(causeScore, noneScore)) + none
        if (theta_j > max) {
          argmax_j = j
          argmax_r =
                  if (noneScore >= Math.max(themeScore, causeScore)) None
                  else if (themeScore >= causeScore) Theme else Cause
          max = theta_j
        }
      }
      if (argmax_j != -1) {
        witnesses(i -> role) = argmax_j -> argmax_r
        dst(i) = s_i + max
      } else {
        dst(i) = s_i
      }
    }
    for (i <- 0 until n) {
      setMaxMarginal(i, Theme, themeMaxMarginals, pairScorer)
      if (allowCause) setMaxMarginal(i, Cause, causeMaxMarginals, pairScorer)
      setMaxMarginal(i, None, noneMaxMarginals, noneScorer) //this should be doable quicker
    }
    //set all values to None by default
    val result = new MutableState
    for (arg <- args) result(arg) = None

    def backPropagate(index: Int, role: String, state: MutableState): Unit = {
      result(args(index)) = role
      for ((lastIndex, lastRole) <- witnesses.get(index -> role)) {
        backPropagate(lastIndex, lastRole, state)
      }
    }

    if (themeMaxMarginals(n - 1) > Math.max(causeMaxMarginals(n - 1), noneMaxMarginals(n - 1))) {
      backPropagate(n - 1, Theme, result)
      result.score = themeMaxMarginals(n - 1)
    } else if (allowCause && causeMaxMarginals(n - 1) > Math.max(themeMaxMarginals(n - 1), noneMaxMarginals(n - 1))) {
      backPropagate(n - 1, Cause, result)
      result.score = causeMaxMarginals(n - 1)
    } else {
      backPropagate(n - 1, None, result)
      result.score = noneMaxMarginals(n - 1)
    }

    result

  }

}
