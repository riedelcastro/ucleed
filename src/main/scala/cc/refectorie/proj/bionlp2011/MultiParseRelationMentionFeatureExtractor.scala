package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.data._
import cc.refectorie.proj.factorieie.annotator.{SnowballStemmer, Annotator}
import collection.mutable.{HashSet, ArrayBuffer, HashMap}
import cc.refectorie.proj.factorieie.util.{Dictionary, HasLogger}
import java.io.File

/**
 * @author riedel
 */

class MultiParseRelationMentionFeatureExtractor extends RelationMentionFeatureExtractor {

  val depNames = Conf.get("depNames", "mcclosky").split(",").map(_.trim)
  val depNamesLocal = Conf.get("depNamesHeads", "mcclosky").split(",").map(_.trim)


  override def triggerFeatures(relMention: RelationMention): Iterable[(Any, Double)] = {
    val sentence = relMention.sentence
    val features = new HashMap[String, Double]
    val prefix = if (isSelfMention(relMention)) "self-" else ""
    if (killerFeature) features(relMention.oid) = 1.0
    else {
      features(prefix + "bias") = 1.0
      val head = relMention.head
      val headWord = head.word.toLowerCase
      val headStem = head.stem.map(_.toLowerCase)
      features(prefix + "headWord: " + headWord) = 1.0
      features(prefix + "headStem: " + headStem) = 1.0
      features(prefix + "headTag: " + head.tag) = 1.0
      if (Conf.get("beforeAfterTrigger", true)) {
        features(prefix + "before trigger: " + relMention.begin.prevOption.map(stemTagHideProteins(_))) = 1.0
        features(prefix + "after trigger: " + relMention.end.nextOption.map(stemTagHideProteins(_))) = 1.0
      }

      for (dict <- dictionaries; if (dict.phrases(head.word.toLowerCase))) {
        features(prefix + "trigger dict: " + dict.id) = 1.0
      }

      if (headWord.startsWith("over")) {
        features(prefix + "over") = 1.0
      }
      if (headWord.startsWith("up"))
        features(prefix + "up") = 1.0
      if (headWord.startsWith("down"))
        features(prefix + "down") = 1.0
      if (headWord.startsWith("co")) {
        features(prefix + "co") = 1.0
      }

      if (headWord.contains('-')) {
        val afterDash = headWord.lastIndexOf('-') + 1
        if (afterDash < headWord.size)
          features(prefix + "headWord: " + headWord.substring(headWord.lastIndexOf('-') + 1)) = 1.0
      }

      if (headWord.startsWith("-")) {
        features(prefix + "headWord: " + headWord.substring(1)) = 1.0
        features(prefix + "headStem: " + headStem.map(_.substring(1))) = 1.0
      }
      //check modifiers
      for (depName <- depNamesLocal;
           dependencyStructure = sentence.getDependencyStructure(depName)) {
        for (modEdge: DependencyStructure#Edge <- dependencyStructure.modifiers(head)) {
          features(prefix + depName + "trigger modifier: %s->%s ".format(modEdge.label, stemTagHideProteins(modEdge.mod))) = 1.0
        }
      }

      if (Conf.get("transcriptionFeatures", false)) {
        features(prefix + "transcription: " + sentence.tokens.exists(t => transcriptionIndicators(t.word.toLowerCase))) = 1.0
      }

      //check heads
      for (depName <- depNamesLocal;
           dependencyStructure = sentence.getDependencyStructure(depName)) {
        for (modEdge: DependencyStructure#Edge <- dependencyStructure.heads(head)) {
          features(prefix + depName + "trigger head: %s->%s ".format(modEdge.label, stemTagHideProteins(modEdge.head))) = 1.0
        }
      }
      for (entMention <- sentence.entityMentionCandidates) {
        for (depName <- depNames) {
          val depPrefix = depName + prefix
          val deps = sentence.getDependencyStructure(depName)
          val paths = deps.shortestPaths()
          val path = paths.getPath(head, entMention.head)
          for (pathFeature <- pathFeatures(path)) {
            features(depPrefix + pathFeature) = 1.0
            if (triggerEntityHeads && depNamesLocal.contains(depName)) {
              for (head <- deps.heads(entMention.head)) {
                features(depPrefix + pathFeature + stemTagHideProteins(head.head)) = 1.0
              }
            }
          }
        }
        for (depName <- depNamesLocal) {
          val depPrefix = depName + prefix
          val deps = sentence.getDependencyStructure(depName)

          if (Conf.get("transcriptionFeatures", false)) {
            for (head <- deps.heads(entMention.head)) {
              features(depPrefix + "transcription prot: " + transcriptionIndicators(head.head.word.toLowerCase)) = 1.0
              features(depPrefix + "transcription prot: " + transcriptionIndicators(head.head.word.toLowerCase)
                + " " + headStem) = 1.0
            }
          }

        }
      }

    }
    if (useStackedFeatures || useStackedTriggers) {
      if (doc2stackingMaps.isEmpty) {
        val stacked = doc2stackingMap(sentence.document)
        val stackPrefix = "stacked:" + stacked.getOrElse(relMention.label, "NoLabel")
        features(stackPrefix) = 1.0
      }
      for (i <- 0 until doc2stackingMaps.size) {
        val stacked = doc2stackingMaps(i)(sentence.document)
        val stackPrefix = "stacked" + i + ":" + stacked.getOrElse(relMention.label, "NoLabel")
        if (useStackedFeaturesConjoin) {
          val current = features.toArray
          for ((key, value) <- current) {
            features(stackPrefix + key) = value
          }
        } else {
          features(stackPrefix) = 1.0
        }
      }
    }

    features
  }


  override def protPairFeatures(arg1: EntityMention, arg2: EntityMention): Iterable[(Any, Double)] = {
    val pairFeatures = new HashMap[String, Double]
    pairFeatures("bias") = 1.0
    val argHead1 = arg1.head
    val argHead2 = arg2.head

    for (depName <- depNames;
         dependencyStructure = argHead1.sentence.getDependencyStructure(depName)) {

      val paths: ShortestPaths = dependencyStructure.shortestPaths()
      val path = normalizePath(paths.getPath(argHead1, argHead2), dependencyStructure)


      //path
      for (pathFeat <- simplePathFeatures(path) ++ path.ngrams(1).flatMap(simplePathFeatures(_)).map("1gram: " + _)
        ++ path.ngrams(2).flatMap(simplePathFeatures(_)).map("2gram: " + _)) {
        pairFeatures(depName + pathFeat) = 1.0
      }
      if (Conf.get("vertexWalk", false)) {
        vertexWalk(depName, pairFeatures, path)
      }
      if (Conf.get("edgeWalk", false)) {
        edgeWalk(depName, pairFeatures, path)
      }
    }
    for (depName <- depNamesLocal;
         dependencyStructure = argHead1.sentence.getDependencyStructure(depName)) {


      for (sharedHead <- dependencyStructure.heads(arg1.head).filter(e1 =>
        dependencyStructure.heads(arg2.head).exists(e2 => e1.head == e2.head))) {
        pairFeatures(depName + "Shared Head") = 1.0
        pairFeatures(depName + "Shared Head:" + stemTagHideProteins(sharedHead.head)) = 1.0
        pairFeatures(depName + "Shared Head:" + stemTagHideProteins(sharedHead.head) + " " + sharedHead.label) = 1.0
      }

      //head arg1

      for (head1 <- dependencyStructure.heads(arg1.head)) {
        pairFeatures(depName + "Head1: -" + head1.label + "->" + stemTagHideProteins(head1.head)) = 1.0
      }
      //head arg1
      for (head2 <- dependencyStructure.heads(arg2.head)) {
        pairFeatures(depName + "Head2: -" + head2.label + "->" + stemTagHideProteins(head2.head)) = 1.0
      }
    }


    if (Conf.get("useProtNames", false)) {
      val prot1 = arg1.phrase.toLowerCase
      val prot2 = arg2.phrase.toLowerCase
      val (p1, p2) = if (prot1 < prot2) (prot1, prot2) else (prot2, prot1)
      pairFeatures("Prots: " + p1 + " " + p2) = 1.0
    }

    if (Conf.get("stackBindPairs", false)) {
      val stacked = doc2stackingMap(argHead1.sentence.document)
      val stackPrefix = "stacked:" + stacked.getOrElse(arg1 -> arg2, "NoBinding")
      pairFeatures(stackPrefix) = 1.0

    }

    //    pairFeatures(arg1.oid + " " + arg2.oid) = 1.0

    //    pairFeatures("argPair: %s %s".format(stemTagHideProteins(arg1Head),stemTagHideProteins(arg2Head))) = 1.0

    pairFeatures("sameWord: " + (argHead1.word == argHead2.word)) = 1.0

    pairFeatures


    //relative position to trigger
    //dependency path between them
    //argument types/words: PROT, non-Prot, word
  }


  override def argPairFeatures(arg1: RelationMentionArgument, arg2: RelationMentionArgument): Iterable[(Any, Double)] = {
    val pairFeatures = new HashMap[String, Double]
    pairFeatures("bias") = 1.0
    val argHead1 = arg1.arg.head
    val argHead2 = arg2.arg.head
    val arg1Position = if (argHead1.indexInDocument < arg1.owner.head.indexInDocument) "L" else "R"
    val arg2Position = if (argHead2.indexInDocument < arg2.owner.head.indexInDocument) "L" else "R"
    pairFeatures("Rel Positions: %s %s".format(arg1Position, arg2Position)) = 1.0

    //pairFeatures("stems: %s %s".format(stemTagHideProteins(arg1.arg.head),stemTagHideProteins(arg2.arg.head))) = 1.0

    val dependencyStructure = arg1.owner.head.sentence.getDependencyStructure.get
    val paths: ShortestPaths = dependencyStructure.shortestPaths()
    val path = normalizePath(paths.getPath(argHead1, argHead2), dependencyStructure)

    for (pathFeat <- simplePathFeatures(path) ++ path.ngrams(1).flatMap(simplePathFeatures(_)).map("1gram: " + _)
      ++ path.ngrams(2).flatMap(simplePathFeatures(_)).map("2gram: " + _)) {
      pairFeatures(pathFeat) = 1.0
    }
    if (Conf.get("vertexWalk", false)) {
      vertexWalk("", pairFeatures, path)
    }
    if (Conf.get("edgeWalk", false)) {
      edgeWalk("", pairFeatures, path)
    }

    //    pairFeatures("argPair: %s %s".format(stemTagHideProteins(arg1Head),stemTagHideProteins(arg2Head))) = 1.0

    pairFeatures


    //relative position to trigger
    //dependency path between them
    //argument types/words: PROT, non-Prot, word
  }

  override def argPairFeaturesForParents(arg1: RelationMentionArgument, arg2: RelationMentionArgument): Iterable[(Any, Double)] = {
    val pairFeatures = new HashMap[String, Double]
    pairFeatures("bias") = 1.0
    val arg1Head = arg1.owner.head
    val arg2Head = arg2.owner.head
    val arg1Position = if (arg1Head.indexInDocument < arg1.arg.head.indexInDocument) "L" else "R"
    val arg2Position = if (arg2Head.indexInDocument < arg2.arg.head.indexInDocument) "L" else "R"
    pairFeatures("Rel Positions: %s %s".format(arg1Position, arg2Position)) = 1.0

    //pairFeatures("stems: %s %s".format(stemTagHideProteins(arg1.arg.head),stemTagHideProteins(arg2.arg.head))) = 1.0

    val dependencyStructure = arg1.owner.head.sentence.getDependencyStructure.get
    val paths: ShortestPaths = dependencyStructure.shortestPaths()
    val path = normalizePath(paths.getPath(arg1.owner.head, arg2.owner.head), dependencyStructure)

    for (pathFeat <- simplePathFeatures(path) ++ path.ngrams(1).flatMap(simplePathFeatures(_)).map("1gram: " + _)
      ++ path.ngrams(2).flatMap(simplePathFeatures(_)).map("2gram: " + _)) {
      pairFeatures(pathFeat) = 1.0
    }
    if (Conf.get("vertexWalk", false)) {
      vertexWalk("", pairFeatures, path)
    }
    if (Conf.get("edgeWalk", false)) {
      edgeWalk("", pairFeatures, path)
    }
    //    paifFeatures("argPair: %s %s".format(stemTagHideProteins(arg1Head),stemTagHideProteins(arg2Head))) = 1.0

    pairFeatures


    //relative position to trigger
    //dependency path between them
    //argument types/words: PROT, non-Prot, word
  }


  override def argFeatures(arg: RelationMentionArgument): Iterable[(Any, Double)] = {
    val prefix = if (isSelfMention(arg.owner)) "self-" else ""
    val triggerHead = arg.owner.head
    val triggerWord = triggerHead.word.toLowerCase
    val triggerStem = triggerHead.stem.map(_.toLowerCase)
    val sentence = triggerHead.sentence

    val argHead = arg.arg.head
    val argFeatures = new HashMap[String, Double]
    if (killerFeature) argFeatures(arg.owner.oid + "-" + arg.arg.oid) = 1.0
    else {
      val target = targetString(arg)
      //      if (arg.argIsEntityMention)
      //        argFeatures(prefix + "argProt: " + argHead.stem) = 1.0
      argFeatures(prefix + "argHead: " + target) = 1.0
      argFeatures(prefix + "bias") = 1.0
      if (target.startsWith("/") || target.startsWith("-")) argFeatures(prefix + "/-") = 1.0
      if (Conf.get("beforeAfterArg", true)) {
        val before = arg.arg.begin.prevOption.map(stemTagHideProteins(_)).toString
        val after = arg.arg.end.nextOption.map(stemTagHideProteins(_)).toString
        //        if (!before.endsWith("RB-)"))
        argFeatures(prefix + "before arg: " + before) = 1.0
        //        if (!after.endsWith("RB-)"))
        argFeatures(prefix + "after arg: " + after) = 1.0
      }

      if (triggerWord.startsWith("-") && arg.arg.end.indexInDocument + 1 == arg.owner.begin.indexInDocument) {
        argFeatures(prefix + "X -[induced]") = 1.0
        //argFeatures(prefix + target + " -[induced] ") = 1.0
      }

      for (depName <- depNamesLocal;
           dependencyStructure = argHead.sentence.getDependencyStructure(depName)) {

        val depPrefix = depName + prefix

        val argHeads = dependencyStructure.heads(argHead)
        if (Conf.get("argHeadFeatures", true)) for (headEdge: DependencyStructure#Edge <- argHeads) {
          argFeatures(prefix + "arg heads: <-%s-%s".format(stemTagHideProteins(headEdge.head), headEdge.label)) = 1.0
          if (Conf.get("argSiblingFeatures", false))
            for (siblingEdge: DependencyStructure#Edge <- dependencyStructure.modifiers(headEdge.head);
                 if (siblingEdge != headEdge)) {
              argFeatures(depPrefix + "arg sibling: <--%s->%s ".format(siblingEdge.label, stemTagHideProteins(siblingEdge.mod))) = 1.0
              argFeatures(depPrefix + "arg sibling: <-%s-%s-%s->%s".format(
                headEdge.label, stemTagHideProteins(headEdge.head), siblingEdge.label, stemTagHideProteins(siblingEdge.mod))) = 1.0
            }
          if (Conf.get("argSiblingReporter", false))
            for (siblingEdge: DependencyStructure#Edge <- dependencyStructure.modifiers(headEdge.head);
                 if (siblingEdge != headEdge)) {
              if (siblingEdge.mod.word.toLowerCase.startsWith("reporter")) {
                argFeatures(depPrefix + "arg sibling: <--%s->%s ".format(siblingEdge.label, stemTagHideProteins(siblingEdge.mod))) = 1.0
                argFeatures(depPrefix + "arg sibling: <-%s-%s-%s->%s".format(
                  headEdge.label, stemTagHideProteins(headEdge.head), siblingEdge.label, stemTagHideProteins(siblingEdge.mod))) = 1.0
              }
            }
        }
      }


      if (Conf.get("closeDistanceFeatures", false)) for (feature <- closeDistanceFeatures(arg)) {
        argFeatures(prefix + feature) = 1.0
      }

      for (depName <- depNames) {
        val structure = triggerHead.sentence.getDependencyStructure(depName)
        val argHeads = structure.heads(argHead)

        val depPrefix = depName + prefix

        val paths: ShortestPaths = structure.shortestPaths()
        //val path = aggressiveNormalizePath(paths.getPath(triggerHead, argHead))
        val unnormalizedPath = paths.getPath(triggerHead, argHead)
        val path = normalizePath(unnormalizedPath, structure)
        val pathFeats = pathFeatures(path) ++
          path.ngrams(2).flatMap(pathFeatures(_)).map("2gram: " + _) ++
          path.ngrams(1).flatMap(pathFeatures(_)).map("1gram: " + _)

        val pathWithArg = pathFeats.map(_ + target)
        val pathWithTrigger = pathFeats.map(_ + triggerStem)
        //        val edgesFrom = path.map(edge => "edgeFrom: " + stemTagHideProteins(edge.from) + edge.edgeString(true))
        //        val edgesTo = path.map(edge => "edgeTo: " + edge.edgeString(true) + stemTagHideProteins(edge.to))
        //val vertexWalk = path.tokens.map(stemTagHideProteins(_)).map(prefix + "vertexWalk: " +  _)
        for (feature <- pathFeats ++ pathWithArg ++ pathWithTrigger) {
          argFeatures(depPrefix + feature) = 1.0
        }
        argFeatures(depPrefix + "path length: " + path.length) = 1.0

        if (useAggressiveNormalization) {
          val aggPath = aggressiveNormalizePath(unnormalizedPath)
          val aggFeats = pathFeatures(aggPath)
          for (feature <- aggFeats) {
            argFeatures(depPrefix + "aggr: " + feature) = 1.0
          }
        }
        if (Conf.get("corefFeatures", false)) {
          //find all pronouns we care about
          val pronouns = sentence.tokens.filter(t => t.indexInDocument > argHead.indexInDocument
            && allPronouns(t.word.toLowerCase))
          for (pronoun <- pronouns) {
            //get path from trigger to pronoun
            val path = normalizePath(paths.getPath(triggerHead, pronoun), structure)
            //get argument heads---this is important for finding out whether the argument could be an antecedent of
            //the pronoun
            for (headEdge: DependencyStructure#Edge <- argHeads; pathFeat <- labelledPathFeatures(path)) {
              argFeatures(depPrefix + "coref (argHead:%s):%s".format(headEdge.label, pathFeat)) = 1.0
              argFeatures(depPrefix + "coref (argHead:%s,arg:%s):%s".format(headEdge.label, target, pathFeat)) = 1.0
              argFeatures(depPrefix + "coref (argHead:%s,trigger:%s):%s".format(headEdge.label, triggerStem, pathFeat)) = 1.0

            }
          }
        }

        if (Conf.get("leftright", false)) {
          for (i <- 0 until path.size) {
            val left = new DependencyPath(path.take(i + 1))
            val right = new DependencyPath(path.takeRight(i + 1))
            argFeatures(depPrefix + "left%d:".format(i) + left.toString(true)) = 1.0
            argFeatures(depPrefix + "left%d-trigger:".format(i) + triggerStem + left.toString(true)) = 1.0
            argFeatures(depPrefix + "right%d:".format(i) + right.toString(true)) = 1.0
            argFeatures(depPrefix + "right%d-lex:".format(i) + right.toString(true) + right.last.from.word.toLowerCase) = 1.0
          }
        }

        if (Conf.get("vertexWalk", false)) {
          vertexWalk(depPrefix, argFeatures, path)
        }
        if (Conf.get("edgeWalk", false)) {
          edgeWalk(depPrefix, argFeatures, path)
        }


      }

    }
    if (useStackedFeatures || useStackedArgs) {
      if (doc2stackingMaps.isEmpty) {
        val stacked = doc2stackingMap(sentence.document)
        val stackPrefix = "stacked:" + stacked.getOrElse(arg.role, "NoLabel")
        argFeatures(stackPrefix) = 1.0
      }
      for (i <- 0 until doc2stackingMaps.size) {
        val stacked = doc2stackingMaps(i)(sentence.document)
        val stackPrefix = "stacked" + i + ":" + stacked.getOrElse(arg.role, "NoLabel")
        if (useStackedFeaturesConjoin) {
          val current = argFeatures.toArray
          for ((key, value) <- current) {
            argFeatures(stackPrefix + key) = value
          }
        } else
          argFeatures(stackPrefix) = 1.0
      }
      //      val current = argFeatures.toArray
      //      for ((key,value) <- current) {
      //        val stacked = doc2stackingMap(sentence.document)
      //        val stackPrefix = "stacked:" + stacked.getOrElse(arg.role, "None")
      //        argFeatures(stackPrefix + key) = value
      //      }
    }

    argFeatures
  }

}

object MultiParseRelationMentionFeatureExtractor
  extends MultiParseRelationMentionFeatureExtractor with CompactFeatureExtractor {
}



