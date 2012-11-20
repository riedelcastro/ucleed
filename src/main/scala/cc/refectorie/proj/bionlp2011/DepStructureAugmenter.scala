package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.annotator.Annotator
import cc.refectorie.proj.factorieie.data.Document

/**
 * Add hyphen and slash dependencies
 * @author sriedel
 */
class DepStructureAugmenter(depName: String,
                            removeHyphenHeads:Boolean = false,
                            removeHyphenMods:Boolean = false) extends Annotator {
  def annotate(doc: Document) {
    for (sentence <- doc.sentences; if (sentence.getDependencyStructureNames(depName))) {
      val depStructure = sentence.getDependencyStructure(depName)
      for ((t1, t2) <- sentence.tokens.dropRight(1) zip sentence.tokens.drop(1)) {
        if (t2.word.startsWith("-")) {
          if (removeHyphenHeads) {
            val heads = depStructure.heads(t1)
            for (edge <- heads) depStructure.removeEdge(edge)
          }
          if (removeHyphenMods) {
            val mods = depStructure.modifiers(t1)
            for (edge <- mods) depStructure.removeEdge(edge)
          }

          depStructure.createEdge(t2, t1, "_hyphen")
        }
        if (t2.word.startsWith("/")) {
          depStructure.createEdge(t2, t1, "_slash")
        }
      }
    }
  }
}

/**
 * For every pair of conjoined tokens A and B, make sure that all children of A
 * are also children of B, and vice versa.
 *
 *
 * @author sriedel
 */
class ConjChildAugmenter(depName: String) extends Annotator {
  def annotate(doc: Document): Unit = {
    for (sentence <- doc.sentences; if (sentence.getDependencyStructureNames(depName))) {
      val depStructure = sentence.getDependencyStructure(depName)
      for (edge <- depStructure.edges) {
        if (edge.label.startsWith("conj")) {
//          for (mod <- depStructure.modifiers(edge.mod); if (mod != edge)) {
//            val existing = depStructure.edges(edge.head, mod.mod)
//            if (existing.isEmpty)
//              depStructure.createEdge(edge.head, mod.mod, mod.label)
//          }
          //check whether modifier is preposition, and to the right of dependent conjunct
          for (mod <- depStructure.modifiers(edge.head); if (mod != edge)) {
            val existing = depStructure.edges(edge.mod, mod.mod)
            if (existing.size == 1 && existing.head == edge)
              depStructure.createEdge(edge.mod, mod.mod, mod.label)
          }
        }
      }

    }
  }
}