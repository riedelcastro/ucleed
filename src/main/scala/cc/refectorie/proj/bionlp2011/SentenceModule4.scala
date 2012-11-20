package cc.refectorie.proj.bionlp2011

import collection.mutable.ArrayBuffer
import cc.refectorie.proj.factorieie.data.{EntityMention, RelationMention, Sentence}

/**
 * @author sriedel
 */
trait SentenceModule4 extends DualDecomposedOneway with Dispatch with GlobalLearner {

  type Observation = Sentence

  def relationMentionModule: Module {type Observation = RelationMention}

  def relationMentionAsTargetModule: Option[Module {type Observation = RelationMention}]

  def siblingModule: Option[EventArgSiblingModule]

  def parentModule: Option[EventArgParentModule]

  def asymmetryModule: Option[AsymmetryModule]

  def bindingModule: Option[PairwiseBindingModule]

  val onlyEntityParents = Conf.get("onlyEntityParents", true)

  override def storeWeights(prefix: String) = {
    relationMentionModule.storeWeights(prefix + "/rel")
    for (m <- relationMentionAsTargetModule)
      m.storeWeights(prefix + "/relAsTarget")
    for (m <- siblingModule)
      m.storeWeights(prefix + "/sibling")
    for (m <- parentModule)
      m.storeWeights(prefix + "/parent")
    for (m <- asymmetryModule)
      m.storeWeights(prefix + "/asym")
    for (m <- bindingModule)
      m.storeWeights(prefix + "/binding")

  }
  override def loadWeights(prefix: String) = {
    relationMentionModule.loadWeights(prefix + "/rel")
    for (m <- relationMentionAsTargetModule)
      m.loadWeights(prefix + "/relAsTarget")
    for (m <- siblingModule)
      m.loadWeights(prefix + "/sibling")
    for (m <- parentModule)
      m.loadWeights(prefix + "/parent")
    for (m <- asymmetryModule)
      m.loadWeights(prefix + "/asym")
    for (m <- bindingModule)
      m.loadWeights(prefix + "/binding")
  }


  def dispatch(obs: Sentence): Iterable[AbstractBatch] = {
    val s = Seq[AbstractBatch](Batch(relationMentionModule, obs.relationMentionCandidates))
    s ++ relationMentionAsTargetModule.map(Batch(_, obs.relationMentionCandidates)).toSeq ++
      parentModule.map(Batch(_, if (onlyEntityParents) obs.entityMentionCandidates else obs.entityMentionCandidates ++ obs.relationMentionCandidates)).toSeq ++
      siblingModule.map(Batch(_, obs.relationMentionCandidates)).toSeq ++
      bindingModule.map(Batch(_, Seq(obs))).toSeq ++
      asymmetryModule.map(Batch(_, Seq(obs))).toSeq
  }


  def consumers(subtask: SubtaskAssignment, producers: scala.Iterable[Producer], state: State) = {
    val result = new ArrayBuffer[Consumer]
    for (producer <- producers) {
      val channel = producer.channel.asInstanceOf[Channel]
      subtask.module match {
        case m: ThemeModule => {
          val trigger = subtask.instance.asInstanceOf[RelationMention]
          if (trigger == channel.trigger) {
            val arg1 = trigger.argumentCandidates.find(_.arg == channel.prot1).get
            val arg2 = trigger.argumentCandidates.find(_.arg == channel.prot2).get
            result += Consumer(subtask, arg1.role, BioNLPConstants.Theme, channel)
            result += Consumer(subtask, arg2.role, BioNLPConstants.Theme, channel)
            result += Consumer(subtask, trigger.label, BioNLPConstants.Binding, channel)
          }
        }
        case _ => {}
      }
    }
    result
  }
  def producers(subtask: SubtaskAssignment, state: State) = {
    val result = new ArrayBuffer[Producer]
    subtask.module match {
      case pw: PairwiseBindingModule => {
        val sentence = subtask.instance.asInstanceOf[Sentence]
        for (variable <- state.domain) {
          variable match {
            case triple@(trigger: RelationMention, prot1: EntityMention, prot2: EntityMention) => {
              if (state.real(triple, true) != 0.0) {
                result += Producer(subtask, triple, true, Channel(trigger, prot1, prot2))
              }
            }
            case _ => {}
          }
        }
      }
      case _ => {}
    }
    result
  }
}
