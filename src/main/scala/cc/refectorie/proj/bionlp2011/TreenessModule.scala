package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.data.{RelationMentionArgument, Sentence}
import annas.graph.{DefaultWeight, DefaultArc, UndirectedGraph}
import annas.graph.util.Prim

/**
 * @author sriedel
 */
trait TreenessModule extends Module with CanAverageParameters with TrainedByEnclosingModule with MakeNoUpdates {
  type Observation = Sentence

  def edgeScorer: LocalLinearScorer[RelationMentionArgument#Role]

}

object Blah {
  def main(args: Array[String]) {
    val a = "A"
    val b = "B"
    val c = "C"
    val d = "D"

    val graph = new UndirectedGraph[String, DefaultArc[String]]();
    graph.addNode(a)
    graph.addNode(b)
    //    graph.addNode(c)
    //    graph.addNode(d)
    graph.addArc(a, b, new DefaultWeight(1.0))
    graph.addArc(b, a, new DefaultWeight(10.0))


    val prim = new Prim(graph)
    val result = prim.execute
    println(result.getArc(a, b).get(0).getWeight)

  }
}