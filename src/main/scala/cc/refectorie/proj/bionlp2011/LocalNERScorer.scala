package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.data.EntityMention
import collection.mutable.HashMap
import cc.refectorie.proj.factorieie.util._

/**
 * NERScorer for Roth data
 * @author sriedel
 */
trait LocalNERScorer extends LinearScorer[EntityMention] {
  def features(variable: EntityMention) = {
    LocalNERFeatureCache.features(variable.oid, {
      val result = new HashMap[String, Double]
      result
    })
  }
}

trait LocalNERModule extends LocalLinearScorerModule {

  type Observation = EntityMention
  def domain = Conf.get[String]("NERDomain").trim.split(",")
  def scorer:LocalNERScorer
}

object LocalNERFeatureCache extends LimitedMemoryFeatureCache {

  type Features = HashMap[String, Double]
  def maxInMemoryCount = 100000
}


