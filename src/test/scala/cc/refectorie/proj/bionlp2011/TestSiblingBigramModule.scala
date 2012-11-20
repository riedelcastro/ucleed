package cc.refectorie.proj.bionlp2011

import org.junit._
import Assert._
import collection.mutable.HashMap


/**
 * @author sriedel
 */
class TestSiblingBigramModule {
  class TestArg(val score: Any => Double)

  import BioNLPConstants._



  class TestModule extends SiblingBigramModule with Perceptron {
    trait ScorerMock[T] extends LocalLinearScorer[T] with TrainedByEnclosingModule {
      def features(variable: T) = Seq("bias" -> 1.0)

      def useAverage: Boolean = false
    }

    type Observation = Seq[TestArg]

    type Arg = TestArg

    def orderedArgs(observation: Observation): Seq[Arg] = observation

    val unigramScorer: LocalLinearScorer[Arg] = new ScorerMock[Arg] {
      override def score(variable: Arg, value: Any) = {
        variable.score(value)
      }

      def domain(variable:Arg) = {
        Seq(Theme,Cause,None)
      }
    }

    val bigramScores = new HashMap[(String,String),Double] {
      override def default(key: (String,String)): Double = 0.0
    }

    val bigramScorer: LocalLinearScorer[(Arg, Arg)] = new ScorerMock[(Arg,Arg)] {
      def domain(variable: (Arg,Arg)) = {
        Seq((Theme->Theme),(Theme->Cause),(Cause->Theme),(Cause->Cause))
      }
      override def score(variable: (Arg, Arg), value: Any) = {
        value match {
          case (l1:String,l2:String) => bigramScores(l1->l2)
          case _ => Double.NegativeInfinity
        }
      }
    }
  }

  @Test
  def testDP {
    val args = Seq(
      new TestArg(Map(Theme -> 1.0, Cause -> 0.0, None -> 0.0)),
      new TestArg(Map(Theme -> 0.0, Cause -> 0.0, None -> 0.0)),
      new TestArg(Map(Theme -> 0.0, Cause -> 0.0, None -> 10.0)))
    val module = new TestModule
    module.bigramScores(Cause -> Theme) = 1.0
    module.bigramScores(Theme -> Cause) = 1.0

    val result = module.inferMAP(args, new Penalties)
    assertEquals(Theme, result(args(0)))
    assertEquals(Cause, result(args(1)))
    assertEquals(None, result(args(2)))

  }

  //@Test
  def testUpdateWeights {
    val args = Seq(
      new TestArg(Map(Theme -> 0.0, Cause -> 0.0, None -> 0.0)),
      new TestArg(Map(Theme -> 0.0, Cause -> 0.0, None -> 0.0)),
      new TestArg(Map(Theme -> 0.0, Cause -> 0.0, None -> 0.0)))
    val guess = new MutableState
    guess(args(0)) = Theme
    guess(args(1)) = None
    guess(args(2)) = Theme

    val gold = new MutableState
    gold(args(0)) = Theme
    gold(args(1)) = Cause
    gold(args(2)) = None

    val module = new TestModule
    module.updateWeights(args,gold,guess, TrainingParameters(1.0,1.0))

    assertEquals(Map(("bias",(Theme,Cause)) -> 1.0, ("bias",(Theme,Theme)) -> -1.0), module.bigramScorer.weights)
    assertEquals(Map(("bias",Theme) -> -1.0, ("bias",None) -> 0.0, ("bias",Cause) -> 1.0), module.unigramScorer.weights)
  }

}