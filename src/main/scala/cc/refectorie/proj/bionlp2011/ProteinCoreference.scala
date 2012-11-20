package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.annotator.Annotator
import collection.mutable.ArrayBuffer
import cc.refectorie.proj.factorieie.data.{EntityMention, DocEntity, Document}

/**
 * @author sriedel
 */
class ProteinCoreference extends Annotator {

  def coref(ent1:EntityMention, ent2:EntityMention):Boolean = {
    if (ent1.phrase.toLowerCase == ent2.phrase.toLowerCase) return true
    for (dep <- Some(ent1.sentence.getDependencyStructure("mcclosky"));
         paths = dep.shortestPaths()){
      val path = paths.getPath(ent1.head,ent2.head)
      if (path.length==1 && path(0).edge.label == "abbrev") return true
    }
    false
  }

  def annotate(doc: Document): Unit = {
    val docEntities = new ArrayBuffer[DocEntity]
    for (sentence <- doc.sentences){
      for (mention <- sentence.entityMentionCandidates){
        var found = false
        for (entity <- docEntities; if (!found)){
          for (other <- entity.mentions.members; if (!found)){
            found = coref(mention, other.value)
            if (found) {
              entity.mentions.add(mention.reference)(null)
            }
          }
        }
        if (!found) {
          val ent = doc.createDocEntity
          ent.mentions.add(mention.reference)(null)
        }
      }
    }

  }
}