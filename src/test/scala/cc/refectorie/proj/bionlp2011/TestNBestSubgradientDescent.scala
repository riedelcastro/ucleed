package cc.refectorie.proj.bionlp2011

import cc.factorie.{Domain, LabelVariable}

/*
* Created by IntelliJ IDEA.
* User: riedelcastro
* Date: 09/03/2011
* Time: 12:07
*/
object TestNBestSubgradientDescent {

  class Label(init: String,override val toString:String) extends LabelVariable[String](init)

  val label1 = new Label("A", "L1")
  val label2 = new Label("B", "L2")

  class SimpleModule(score1: Double, score2: Double, override val toString:String)
    extends Module with MakeNoUpdates with TrainedByEnclosingModule {
    type Observation = Any

    def inferMAP(obs: Observation, penalties: Penalties): State = {
      println("====")
      println(this)
      println(penalties)
      val result = new MutableState
      val s1 = score1 + penalties.penalty(label1, "A") - penalties.penalty(label1, "B")
      val s2 = score2 + penalties.penalty(label2, "A") - penalties.penalty(label2, "B")
      if (s1 > s2) {
        result(label1) = "A"
        result(label2) = "B"
        result.score = s1
      } else {
        result(label1) = "B"
        result(label2) = "A"
        result.score = s2
      }
      println(result)
      println("Score: " + result.score)
      result
    }
  }

  class Combined(m1: SimpleModule, m2: SimpleModule)
    extends DualDecomposedOneway with MakeNoUpdates with TrainedByEnclosingModule
    with DualIncreaseCountStepSize {

    def subgradientSteps = 100
    def initialStepSize = 1.0
    type Observation = Any
    def decompose(obs: Observation) = Seq(Assign(m1, obs), Assign(m2, obs))
    def consumers(subtask: SubtaskAssignment, producers: Iterable[Producer], state: State) = Seq.empty
    def producers(subtask: SubtaskAssignment, state: State) = Seq.empty
  }

  def main (args: Array[String]) {
    Domain[Label] += "A"
    Domain[Label] += "B"
    val m1 = new SimpleModule(3.0, -3.0, "M1")
    val m2 = new SimpleModule(-1.0, 1.0, "M2")
    val combined = new Combined(m1,m2)
    val nBest = combined.nBest(2, new Penalties)
    for (best <- nBest){
      println("====")
      println(best.score)
      println(best)
    }
  }

}