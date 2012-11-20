package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.data.EntityMention

/**
 * @author sriedel
 */
trait ProteinChain extends Module {

  import BioNLPConstants._

  type Element
  type Observation = Seq[Element]
  type Arg

  def args(element:Element):Seq[Arg]

  def unigramScorer:LocalLinearScorer[Element]
  def bigramScorer:LocalLinearScorer[(Element,Element)]

  def updateWeights(obs: Observation, gold: State, guess: State, param: TrainingParameters): Unit = {

    val elements = obs
    val goldStates = for (element <- elements; if (args(element).exists(gold(_) == Theme))) yield element
    val guessStates = for (element <- elements; if (args(element).exists(guess(_) == Theme))) yield element

    unigramScorer.batchUpdateWeights(goldStates.map(Assignment(_,true)),guessStates.map(Assignment(_,true)), param)

    val goldPairs = for ((e1,e2) <- goldStates.take(goldStates.length - 1) zip goldStates.drop(1)) yield
      Assignment(e1->e2,true)
    val guessPairs = for ((e1,e2) <- guessStates.take(guessStates.length - 1) zip guessStates.drop(1)) yield
      Assignment(e1->e2,true)

    bigramScorer.batchUpdateWeights(goldPairs, guessPairs, param)
  }


  /**
   * This model has several optima, and can pick one of these based on an existing solution.
   * For each protein the models decides whether it should be a Theme or not. If it is a Theme,
   * the model can pick an arbitrary set of arguments as Themes. If the proposal state
   * contains a theme already, we pick the proposal. If not, we augment the proposal with additional
   * Theme arguments (namely, all arguments in the proposal which are not Causes, if
   * no none-Cause arg is there, turn all Causes into Themes).
   */
  def inferMap(observation: Observation, proposal: State, penalties: Penalties): State = {
    val elements = observation
    val n = elements.size
    val maxMarginals = Array.fill(n+1)(0.0)
    val witnesses = Array.fill(n+1)(0)
    val states = Array.fill(n+1)(false)

    for (i <- 1 until n){
      val s = unigramScorer.score(elements(i),true)
      var max = Double.NegativeInfinity
      var witness = -1
      for (j <- 0 until i) {
        val value = maxMarginals(j) + bigramScorer.score(elements(j)->elements(i), true)
        if (value > max) {
          max = value
          witness = j
        }
      }
      maxMarginals(i) = max
      witnesses(i) = witness
    }

    //now create result
    var l = n - 1
    while (l >= 0){
      if (maxMarginals(l) > 0){
        states(l) = true
        l = witnesses(l)
      } else {
        l -= 1
      }
    }

    val result = new MutableState
    for (i <- 1 until n) {
      val element = elements(i)
      val args = this.args(element)
      if (states(i)) {
        if (!args.exists(proposal(_) == Theme)) {
          for (arg <-args){
            result(arg) = proposal(arg)
          }
        } else {
          for (arg <- args){
            result(arg) = Theme
          }
        }
      } else {
        for (arg <- args){
          result(arg) = None
        }
      }
    }
    result
  }

}