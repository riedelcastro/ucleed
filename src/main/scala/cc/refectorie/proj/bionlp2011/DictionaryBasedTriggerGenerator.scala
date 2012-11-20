package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.util.{Dictionary, HasLogger}
import cc.refectorie.proj.factorieie.data.{Token, Document}
import cc.refectorie.proj.factorieie.annotator.{SnowballStemmer, Annotator}
import collection.mutable.ArrayBuffer

/**
 * Creates new relation mentions based on match of token content with some dictionary of words.
 * @author sriedel
 */
class DictionaryBasedTriggerGenerator(val triggerDict: Dictionary)
        extends PredicateBasedTriggerGenerator(DictionaryBasedTriggerGenerator.check(_,triggerDict)) {
}

class DictionaryBasedTriggerGenerator2(val triggerDict: Dictionary, check:Token => Boolean)
        extends PredicateBasedTriggerGenerator(t => check(t) && DictionaryBasedTriggerGenerator.check(t,triggerDict)) {
}

class DictionaryBasedEntityGenerator2(val entityDict: Dictionary, check:Token => Boolean)
        extends PredicateBasedEntityGenerator(t => check(t) && DictionaryBasedTriggerGenerator.check(t,entityDict)) {
}


object DictionaryBasedTriggerGenerator {
  def check(token: Token, triggerDict:Dictionary) : Boolean = {
    //if (token.word.size == 1) return false
    val toCheck = new ArrayBuffer[String]
    val word = token.word.toLowerCase
    toCheck += word
    if (word.contains('-')){
      toCheck += word.substring(word.lastIndexOf('-') + 1)
    }
    if (word.startsWith("over")){
      toCheck += word.substring(4)
    }
    if (word.startsWith("up")){
      toCheck += word.substring(2)
    }
    toCheck.exists(word => triggerDict.phrases(SnowballStemmer.stem(word)))
  }  
}

/**
 * Tests token for some property and adds a relation mention if property is present.
 */
class PredicateBasedTriggerGenerator(predicate: Token => Boolean) extends Annotator with HasLogger {

  def annotate(doc: Document): Unit = {
    //remember token to entity mention mapping
    val entityMentionHeads = doc.allEntityMentionCandidates.flatMap(_.span).toSet
    for (token <- doc.tokens) {
      if (predicate(token) && !entityMentionHeads(token)) {
        val relMention = token.sentence.createRelationMention(token)
        relMention.isTriggerCentric = true
      }
    }
    logger.info("Triggers generated for " + doc.id)
  }

}

class PredicateBasedEntityGenerator(predicate: Token => Boolean, tag:String= BioNLPConstants.EntityTag) extends Annotator with HasLogger {

  def annotate(doc: Document): Unit = {
    //remember token to entity mention mapping
    val entityMentionHeads = doc.allEntityMentionCandidates.filter(!_.tags(tag)).flatMap(_.span).toSet
    var id = 10000
    for (token <- doc.tokens) {
      if (predicate(token) && !entityMentionHeads(token)) {
        val entityMention = token.sentence.createEntityMention(token)
        entityMention.entityType.trueValue = BioNLPConstants.None
        entityMention.exists.trueValue = false
        entityMention.tags += tag
        entityMention.entityKey = Some("T" + id)
        id += 1
      }
    }
    logger.info("Entities generated for " + doc.id)
  }

}


object ArgumentCandidateGenerator extends Annotator with HasLogger {
  import BioNLPConstants._
  def annotate(doc: Document): Unit = {
    for (sentence <- doc.sentences; relMention <- sentence.relationMentionCandidates) {
      //attach each entity mention to it
      for (mention <- sentence.entityMentionCandidates ++ sentence.relationMentionCandidates;
           if (mention != relMention)) {
        val arg = relMention.createArgument(mention)
        if (arg.argIsEntityMention && arg.entityMention.tags(EntityTag)) {
          arg.role.trueValue = NoSite
        }
      }
    }
    for (sentence <- doc.sentences; relMention <- sentence.relationMentionCandidates) {
      //in addition, create a "self" event that can only have the current rel mention as argument
      val self = sentence.createRelationMention(relMention.head)
      self.createArgument(relMention)
    }
    logger.info("Argument candidates generated for " + doc.id)
  }
}