package cc.refectorie.proj.bionlp2011

import collection.mutable.{HashSet, ArrayBuffer, HashMap}
import cc.refectorie.proj.factorieie.annotator.SnowballStemmer
import BioNLPConstants._
import cc.refectorie.proj.factorieie.data.{RelationMentionArgument, RelationMention, EntityMention, Document}

class PredictionStats(val doc: Document) extends HashMap[Any, Any] {
  self =>

  override def toString = {
    proteinClusters.mkString("\n") + "\n" + proteinClusterPairs.filter(_.boundMentions.size > 0).mkString("\n") +
      "\n" + triggerClusters.mkString("\n")
  }

  class ProteinCluster extends ArrayBuffer[EntityMention] {
    var representations = new HashSet[String]

    lazy val stats: Seq[Seq[(String, String, String)]] = {
      for (mention <- this) yield {
        def roleEventPair(arg: RelationMentionArgument) =
          (self.getOrElse(arg.role, None).toString,
            self.getOrElse(arg.owner.label, None).toString,
            SnowballStemmer.stem(arg.owner.head.word.toLowerCase))
        mention.argumentInCandidates.map(arg => roleEventPair(arg)).toSet.toSeq.sorted
      }
    }


    override def toString() = {
      representations.mkString(",") + "\n" +
        stats.map(s => "  %s".format(s.filter(_._1 != None).mkString(","))).mkString("\n")
    }

    def statsWithout(without: Int) = {
      (stats.take(without) ++ stats.drop(without + 1))
    }

    def zippedStatsWithout(without: Int) = {
      val zipped = stats.zipWithIndex
      zipped.take(without) ++ zipped.drop(without + 1)
    }

    def previousStats(without: Int) = stats.take(without).filter(_.exists(_._1 != None)).lastOption


  }

  class TriggerCluster extends ArrayBuffer[RelationMention] {
    var representations = new HashSet[String]

    lazy val stats: Seq[String] = {
      for (trigger <- this) yield {
        self.getOrElse(trigger.label, None).toString
      }
    }
    def statsWithout(without: Int) = (stats.take(without) ++ stats.drop(without + 1))

    def previousStats(without: Int) = stats.take(without).filter(_ != None).lastOption

    override def toString() = {
      representations.mkString(",") + "\n  " +
        stats.mkString(",")
    }
  }

  class ProteinClusterPair(val cluster1: ProteinCluster, val cluster2: ProteinCluster) {
    lazy val boundMentions: Set[(EntityMention, EntityMention)] = {
      for (prot1 <- cluster1; prot2 <- cluster2;
           if (prot1.sentence == prot2.sentence && prot1.head.indexInDocument < prot2.head.indexInDocument);
           if (self.getOrElse(prot1 -> prot2, false) == true)) yield {
        prot1 -> prot2
      }
    }.toSet

    def boundWithout(prot1: EntityMention, prot2: EntityMention): Boolean = {
      if (boundMentions(prot1 -> prot2)) boundMentions.size > 1 else boundMentions.size > 0
    }
    override def toString =
      cluster1.representations.mkString(",") +
        " - " + cluster2.representations.mkString(",") + ": " + boundMentions.size
  }

  lazy val proteinPairsInBindings = {
    val result = new HashSet[(ProteinCluster, ProteinCluster)]
    for (sentence <- doc.sentences) {
      for (prot1 <- sentence.entityMentionCandidates;
           prot2 <- sentence.entityMentionCandidates;
           if (!prot1.tags(EntityTag) && !prot2.tags(EntityTag));
           if (prot1.head.indexInDocument < prot2.head.indexInDocument)) {
        if (self.getOrElse(prot1 -> prot2, false) == true) {
          result += protein2Cluster(prot1)._2 -> protein2Cluster(prot2)._2
        }
      }
    }
    result
  }

  lazy val proteinClusterPairs = {
    for (c1 <- proteinClusters; c2 <- proteinClusters; if (c1 != c2)) yield new ProteinClusterPair(c1, c2)
  }
  lazy val proteinClusters = gatherProteinClusters
  lazy val triggerClusters = gatherTriggerClusters
  lazy val protein2Cluster: Map[EntityMention, (Int, ProteinCluster)] = {
    for (cluster <- proteinClusters; (member, index) <- cluster.zipWithIndex) yield member -> (index, cluster)
  }.toMap
  lazy val trigger2Cluster: Map[RelationMention, (Int, TriggerCluster)] = {
    for (cluster <- triggerClusters; (member, index) <- cluster.zipWithIndex) yield member -> (index, cluster)
  }.toMap

  lazy val protPair2Cluster: Map[(EntityMention, EntityMention), ProteinClusterPair] = {
    for (cluster <- proteinClusterPairs; pair <- cluster.boundMentions) yield pair -> cluster
  }.toMap


  def gatherTriggerClusters: Seq[TriggerCluster] = {
    val result = new ArrayBuffer[TriggerCluster]
    for (sentence <- doc.sentences) {
      for (trigger <- sentence.relationMentionCandidates) {
        val phrase = SnowballStemmer.stem(trigger.phrase.toLowerCase)
        val existingOption = result.find(_.representations(phrase))
        for (existing <- existingOption) {
          existing += trigger
          existing.representations += phrase
        }
        if (existingOption.isEmpty) {
          val newCluster = new TriggerCluster
          newCluster.representations += phrase
          newCluster += trigger
          result += newCluster
        }
      }
    }
    result
  }

  def proteinPhrase(text: String) = {
    if (text.startsWith("-") || text.startsWith("/")) text.drop(1).toLowerCase else text.toLowerCase
  }

  def gatherProteinClusters: Seq[ProteinCluster] = {
    val aliases = new HashMap[String, Set[String]] {
      override def default(key: String) = Set.empty
    }
    for (sentence <- doc.sentences) {
      val dep = sentence.getDependencyStructure("mcclosky")
      for (entityMention <- sentence.entityMentionCandidates;
           if (!entityMention.tags(BioNLPConstants.EntityTag))) {
        for (headDep <- dep.heads(entityMention.head);
             if (headDep.label == "abbrev")) {
          // || headDep.label == "appos")) {
          for (other <- sentence.entityMentionCandidates.find(_.head == headDep.head)) {
            aliases(other.phrase.toLowerCase) =
              aliases(other.phrase.toLowerCase) ++ Seq(entityMention.phrase.toLowerCase)
          }
        }
      }
    }
    val result = new ArrayBuffer[ProteinCluster]
    for (sentence <- doc.sentences) {
      for (entityMention <- sentence.entityMentionCandidates;
           if (!entityMention.tags(BioNLPConstants.EntityTag))) {
        val phrase = proteinPhrase(entityMention.phrase)
        val existingOption = result.find(cluster => cluster.representations(phrase))
        for (existing <- existingOption) {
          existing += entityMention
          existing.representations ++= Seq(phrase) ++ aliases(phrase)
        }
        if (existingOption.isEmpty) {
          val newCluster = new ProteinCluster
          newCluster += entityMention
          newCluster.representations ++= Seq(phrase) ++ aliases(phrase)
          result += newCluster
        }
      }
    }
    result

  }

}