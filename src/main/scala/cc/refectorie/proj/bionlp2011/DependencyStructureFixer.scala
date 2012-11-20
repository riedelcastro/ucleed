package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.annotator.Annotator
import cc.refectorie.proj.factorieie.data.{DependencyStructure, Token, Document}
import cc.refectorie.proj.factorieie.util.HasLogger
import collection.mutable.{HashSet, ArrayBuffer}

/**
 * @author sriedel
 */
object DependencyStructureFixer extends Annotator with HasLogger {
  def sameSide(target: Token, t1: Token, t2: Token) = {
    t1.indexInDocument < target.indexInDocument && t2.indexInDocument < target.indexInDocument ||
            t1.indexInDocument > target.indexInDocument && t2.indexInDocument > target.indexInDocument
  }

  def annotate(doc: Document): Unit = {
    var changeCount = 0
    for (sentence <- doc.sentences) {
      for (name <- sentence.getDependencyStructureNames) {
        val deps = sentence.getDependencyStructure(name)
        val toRemove = new HashSet[deps.Edge]
        val toAdd = new ArrayBuffer[(Token, Token, String)]
        for (edge <- deps.edges) {
          if (edge.label.startsWith("conj")) {
            val headProt = sentence.entityMentionCandidates(edge.head)
            val modProt = sentence.entityMentionCandidates(edge.mod)
            //todo: should also check whether the non-prot may not really be a protein, that is just not annotated
            if (headProt.size != modProt.size) {
              val (prot, nonProt) = if (headProt.size > 0) (edge.head, edge.mod) else (edge.mod, edge.head)
              if (!nonProt.word.head.isUpper || nonProt.indexInSentence == 0) {
                val protChildren = deps.modifiers(nonProt).filter(e =>
                  e != edge && !sentence.entityMentionCandidates(e.mod).isEmpty &&
                          sameSide(nonProt, e.mod, prot)).toList.sortBy(_.mod.indexInDocument)
                for (protChild <- protChildren.headOption) {
                  toRemove += edge
                  toAdd += ((nonProt, prot, protChild.label))
                  toAdd += ((prot, protChild.mod, edge.label))
                  //remove all previous heads of the prot token
                  for (previousHead <- deps.heads(prot); if (previousHead != edge)) {
                    toRemove += previousHead
                  }
                  changeCount += 1
                }
              }
            }
          }
        }
        for (edge <- toRemove) {
          deps.removeEdge(edge)
        }
        for ((head, mod, label) <- toAdd) {
          deps.createEdge(head, mod, label)
        }
      }
    }
    logger.info("Changed %d conjuncts".format(changeCount))
  }
}