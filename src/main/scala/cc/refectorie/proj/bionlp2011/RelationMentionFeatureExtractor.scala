package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.data._
import cc.refectorie.proj.factorieie.annotator.{SnowballStemmer, Annotator}
import collection.mutable.{HashSet, ArrayBuffer, HashMap}
import java.io.File
import cc.refectorie.proj.factorieie.util.{Counting, Dictionary, HasLogger}

/**
 * @author riedel
 */

class RelationMentionFeatureExtractor extends Annotator with HasLogger {
  var killerFeature = Conf.get("killerFeature", false)
  val proteinTokens = new HashSet[Token]

  val dictionaries = new ArrayBuffer[Dictionary]

  //used for predictions of other models to be used as features
  lazy val stackingDirs = Conf.get("stackingDirs", "").split(",").map(_.trim).map(new File(_))
  val stackingLoaders = stackingDirs.map(dir => new BioNLPGoldAnnotator(dir))
  val stackingLoader = new BioNLPGoldAnnotator(new File(Conf.get("stackingDir", "/tmp/")))
  val doc2stackingMaps = stackingLoaders.map(loader => MemoUtil.concurrentHashMapMemo[Document, PredictionStats] {
    document => {
      val map = loader.createMap(document);
      //println(map);
      map
    }
  })
  val doc2stackingMap = MemoUtil.concurrentHashMapMemo[Document, PredictionStats] {
    document => stackingLoader.createMap(document)
  }


  val useStackedFeatures = Conf.get("useStacked", false)
  val useStackedFeaturesConjoin = Conf.get("useStackedConjoin", false)
  val useStackedTriggers = Conf.get("useStackedTriggers", false)
  val useStackedArgs = Conf.get("useStackedArgs", false)


  def isSelfMention(mention: SentenceMention[_]) = {
    mention match {
      case rm: RelationMention => rm.argumentCandidates.size == 1 &&
        rm.argumentCandidates.head.arg.head == rm.head
      case _ => false
    }
  }

  val removableEdgeLabels = Set("appos", "prep_such_as", "abbrev",
    "prep_including", "prep_like", "num")
  //prep_like, number, conj_and?

  val removableEdgeLabelsAgressive = removableEdgeLabels ++ Set("conj_and", "conj", "conj_or")

  val removableNNHeads = Set("protein", "gene", "mRNA")

  var hideProteins = Conf.get("hideProteins", false)


  val conjChildNormalization = Conf.get("conjChildNormalize", false)
  val conjPrepOfNormalization = Conf.get("conjPrepOfNormalize", false)


  def normalizePath(path: DependencyPath, depStructure: DependencyStructure): DependencyPath = {
    //remove apposition etc
    val filtered = path.self.filter(edge => !removableEdgeLabels(edge.edge.label))

    //normalize things like "X protein/mRNA/gene" to "X"
    var nnNormalized = if (filtered.size > 0 && filtered.last.edge.label == "nn"
      && removableNNHeads(filtered.last.edge.head.word.toLowerCase)) filtered.dropRight(1)
    else filtered

    if (conjChildNormalization) {
      val toRemove = new HashSet[DependencyStructure#Edge]
      val bigrams = nnNormalized.dropRight(1) zip nnNormalized.drop(1)
      for ((e1, e2) <- bigrams) {
        val (conj, prep) = if (e1.edge.label.startsWith("conj")) (e1.edge, e2.edge) else (e2.edge, e1.edge)
        if (conj.label.startsWith("conj") && prep.label.startsWith("prep") &&
          conj.head == prep.head && prep.mod.indexInDocument > conj.mod.indexInDocument &&
          depStructure.modifiers(conj.mod).forall(!_.label.startsWith("prep"))) {
          toRemove += conj
        }
      }
      nnNormalized = nnNormalized.filter(e => !toRemove(e.edge))
    }

    if (conjPrepOfNormalization) {
      val toChange = new HashSet[DependencyStructure#Edge]
      val bigrams = nnNormalized.dropRight(1) zip nnNormalized.drop(1)
      for ((e1, e2) <- bigrams) {
        val (conj, prep) = if (e1.edge.label.startsWith("conj")) (e1.edge, e2.edge) else (e2.edge, e1.edge)
        if (conj.label.startsWith("conj") && prep.label.startsWith("prep") &&
          conj.mod == prep.head &&
          depStructure.modifiers(conj.head).exists(_.label == "nn")) {
          toChange += conj
          logger.debug("Found conj-prep structure to change")
        }
      }
      nnNormalized = nnNormalized.map(e =>
        if (toChange(e.edge)) PathEdge(e.edge.copy(label = e.edge.label + "prep"), e.forward) else e)
    }



    //if no edge is left use the original path.
    if (nnNormalized.size == 0) path else new DependencyPath(nnNormalized)
  }

  def aggressiveNormalizePath(path: DependencyPath): DependencyPath = {


    //remove apposition etc.
    val filtered = path.self.filter(edge => !removableEdgeLabelsAgressive(edge.edge.label))

    //normalize things like "X protein/mRNA/gene" to "X"
    val nnNormalized = if (filtered.size > 0 && filtered.last.edge.label == "nn"
      && removableNNHeads(filtered.last.edge.head.word.toLowerCase)) filtered.dropRight(1)
    else filtered

    //if no edge is left use the original path.
    if (nnNormalized.size == 0) path else new DependencyPath(nnNormalized)
  }

  val triggerEntityHeads = Conf.get("triggerEntityHeads", false)


  def normalizeDepLabel(label: String): String = {
    label match {
      case "prep_through" => "prep_by"
      case "prep_via" => "prep_by"
      case "prepc_by" => "prep_by"
      //      case "prep_within" => "prep_in"
      case x => x
    }
  }

  //val triggerDictionary = new Dictionary(Conf.get("triggerDict", "dictionaries/chun-event.txt"))

  val transcriptionIndicators = Set("mrna", "mrnas")

  def triggerFeatures(relMention: RelationMention): Iterable[(Any, Double)] = {
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
      val dependencyStructure = sentence.getDependencyStructure.get
      for (modEdge: DependencyStructure#Edge <- dependencyStructure.modifiers(head)) {
        features(prefix + "trigger modifier: %s->%s ".format(modEdge.label, stemTagHideProteins(modEdge.mod))) = 1.0
      }

      if (Conf.get("transcriptionFeatures", false)) {
        features(prefix + "transcription: " + sentence.tokens.exists(t => transcriptionIndicators(t.word.toLowerCase))) = 1.0
      }

      //check heads
      for (modEdge: DependencyStructure#Edge <- dependencyStructure.heads(head)) {
        features(prefix + "trigger head: %s->%s ".format(modEdge.label, stemTagHideProteins(modEdge.head))) = 1.0
      }
      for (entMention <- sentence.entityMentionCandidates) {
        for (depName <- sentence.getDependencyStructureNames) {
          val deps = sentence.getDependencyStructure(depName)
          val paths = deps.shortestPaths()
          val path = paths.getPath(head, entMention.head)
          for (pathFeature <- pathFeatures(path)) {
            features(prefix + pathFeature) = 1.0
            if (triggerEntityHeads) {
              for (head <- deps.heads(entMention.head)) {
                features(prefix + pathFeature + stemTagHideProteins(head.head)) = 1.0
              }
            }
          }
          if (Conf.get("transcriptionFeatures", false)) {
            for (head <- deps.heads(entMention.head)) {
              features(prefix + "transcription prot: " + transcriptionIndicators(head.head.word.toLowerCase)) = 1.0
              features(prefix + "transcription prot: " + transcriptionIndicators(head.head.word.toLowerCase)
                + " " + headStem) = 1.0
            }
          }
        }
      }
      if (Conf.get("stackedDiscourse", false)) {
        val stackedFeats = new HashMap[String, Double]
        def addDiscourseFeats(i: Int, label: (String, String, String), discPrefix: String = "discourse-arg",
                              statIndex: Int = 100000000): Unit = {
          val stackPrefix = prefix + discPrefix + i + ":" + label._1 + label._2
          val stackPrefix2 = prefix + discPrefix + i + ":" + label._1
          val stackPrefix3 = prefix + discPrefix + i
          stackedFeats(stackPrefix) = 1.0
          stackedFeats(stackPrefix2) = 1.0
          stackedFeats(stackPrefix3) = 1.0
          stackedFeats(prefix + label._1 + label._2 + "-sh " + (headStem.get == label._3)) = 1.0
          stackedFeats(prefix + label._1 + "-sh " + (headStem.get == label._3)) = 1.0
          if (statIndex < 2) {
            stackedFeats(stackPrefix + "-" + statIndex) = 1.0
          }
        }
        for (i <- 0 until doc2stackingMaps.size) {
          val stacked = doc2stackingMaps(i)(sentence.document)
          val (index, cluster) = stacked.trigger2Cluster(relMention)
          val stats = cluster.statsWithout(index)
          for (label: String <- stats.toSet) {
            val stackPrefix = prefix + "discourse " + i + ":" + label
            stackedFeats(stackPrefix) = 1.0
          }
          for (entMention <- sentence.entityMentionCandidates) {
            val (protIndex, protCluster) = stacked.protein2Cluster(entMention)
            val protStats = protCluster.zippedStatsWithout(protIndex)
            for ((stat, statIndex) <- protStats; label <- stat; if (label._1 != BioNLPConstants.None)) {
              addDiscourseFeats(i, label, statIndex = statIndex)
            }
            val markovProtStats = protCluster.previousStats(protIndex)
            for (stat <- markovProtStats; label <- stat; if (label._1 != BioNLPConstants.None)) {
              addDiscourseFeats(i, label, "discourse-markov")
            }

          }

        }
        if (Conf.get("conjoinDiscourse", false)) {
          for (feat <- Seq("", headStem); (thatFeat, thatValue) <- stackedFeats) {
            features(thatFeat + " " + feat) = thatValue
          }
        } else {
          features ++= stackedFeats
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

  def targetString(arg: RelationMentionArgument): String = {
    arg.arg match {
      case em: EntityMention => "PROT"
      case rm: RelationMention => SnowballStemmer.stem(rm.head.word.toLowerCase)
    }
  }

  def stemTagHideProteins(token: Token): String = {
    //    "%s/%s".format(token.stem.map(_.toLowerCase),token.tag)
    //todo: this works better ...
    //    if (proteinTokens(token)) "PROT" else "%s/%s".format(token.stem.map(_.toLowerCase), token.tag)
    if (hideProteins && isProteinToken(token)) "PROT" else "%s/%s".format(token.stem.map(_.toLowerCase), token.tag)
  }

  def pathFeatures(path: DependencyPath): Seq[String] = {
    //    Seq(
    //      path.toString(true),
    //      path.toString(false),
    //      path.toString(true, t => stemTagHideProteins(t)),
    //      path.toString(false, t => stemTagHideProteins(t)))
    Seq(
      path.toString(t => "", label => normalizeDepLabel(label)),
      path.toString(t => "", label => ""),
      path.toString(t => stemTagHideProteins(t), label => normalizeDepLabel(label)),
      path.toString(t => stemTagHideProteins(t), label => ""))
  }

  def labelledPathFeatures(path: DependencyPath): Seq[String] = {
    //    Seq(
    //      path.toString(true),
    //      path.toString(false),
    //      path.toString(true, t => stemTagHideProteins(t)),
    //      path.toString(false, t => stemTagHideProteins(t)))
    Seq(
      path.toString(t => "", label => normalizeDepLabel(label)),
      path.toString(t => stemTagHideProteins(t), label => normalizeDepLabel(label)))
  }


  def simplePathFeatures(path: DependencyPath): Seq[String] = {
    //    Seq(
    //      path.toString(true),
    //      path.toString(false),
    //      path.toString(true, t => stemTagHideProteins(t)),
    //      path.toString(false, t => stemTagHideProteins(t)))
    Seq(
      path.toString(t => "", label => normalizeDepLabel(label)),
      path.toString(t => "", label => ""))
  }


  def pathFeatures(arg1: RelationMention, arg2: SentenceMention[_]): Seq[String] = {
    val result = new ArrayBuffer[String]
    val triggerHead = arg1.head
    for (depName <- triggerHead.sentence.getDependencyStructureNames;
         paths: ShortestPaths = triggerHead.sentence.getDependencyStructure(depName).shortestPaths();
         path = paths.getPath(arg1.head, arg2.head)) {
      result ++= pathFeatures(path)
      for (n <- 2 until 5; ngram <- path.ngrams(n)) {
        result ++= pathFeatures(ngram)
      }
    }
    result
  }

  def closeDistanceFeatures(arg: RelationMentionArgument): Seq[String] = {
    if (arg.argIsRelationMention) Seq.empty
    else {
      val result = new ArrayBuffer[String]
      val triggerHead = arg.owner.head
      val triggerWord = triggerHead.word.toLowerCase
      val triggerStem = triggerHead.stem.map(_.toLowerCase)
      val target = targetString(arg)

      //[ARG] [trigger]
      val isLeft = arg.owner.begin.prevOption == Some(arg.arg.end)

      if (isLeft) {
        result += "[PROT] [trigger]"
        result += "[PROT] " + triggerStem
        if (arg.arg.begin.word.startsWith("-") || arg.arg.begin.word.startsWith("/")) {
          result += "[-PROT] [trigger]"
          result += "[-PROT] " + triggerStem
        }
        if (triggerWord.startsWith("-")) {
          result += "[PROT] [-trigger]"
          result += "[PROT] " + triggerStem.map(_.drop(1))
        }
      }

      //[ARG] PROT PROT ... PROT [trigger]
      val isLeftBeforeProts = arg.arg.end.indexInDocument < arg.owner.begin.indexInDocument - 1 &&
        arg.owner.sentence.document.tokens.slice(arg.arg.end.indexInDocument + 1, arg.owner.begin.indexInDocument).
          forall(proteinTokens(_))
      if (isLeftBeforeProts) {
        result += "[PROT] PROT ... PROT [trigger]"
        result += "[PROT] PROT ... PROT " + triggerStem
      }
      result
    }
    //[PROTA] PROTB [trigger]
    //[PROTA] -PROTB [trigger]
  }

  def protPairFeatures(arg1: EntityMention, arg2: EntityMention): Iterable[(Any, Double)] = {
    val pairFeatures = new HashMap[String, Double]
    pairFeatures("bias") = 1.0
    val argHead1 = arg1.head
    val argHead2 = arg2.head
    val dependencyStructure = argHead1.sentence.getDependencyStructure.get
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
    pairFeatures("sameWord: " + (argHead1.word == argHead2.word)) = 1.0

    for (sharedHead <- dependencyStructure.heads(arg1.head).filter(e1 =>
      dependencyStructure.heads(arg2.head).exists(e2 => e1.head == e2.head))) {
      pairFeatures("Shared Head") = 1.0
      pairFeatures("Shared Head:" + stemTagHideProteins(sharedHead.head)) = 1.0
      pairFeatures("Shared Head:" + stemTagHideProteins(sharedHead.head) + " " + sharedHead.label) = 1.0
    }

    //head arg1
    for (head1 <- dependencyStructure.heads(arg1.head)) {
      pairFeatures("Head1: -" + head1.label + "->" + stemTagHideProteins(head1.head)) = 1.0
    }
    //head arg1
    for (head2 <- dependencyStructure.heads(arg2.head)) {
      pairFeatures("Head2: -" + head2.label + "->" + stemTagHideProteins(head2.head)) = 1.0
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

    if (Conf.get("stackedDiscourse", false)) {
      for (i <- 0 until doc2stackingMaps.size) {
        val stacked = doc2stackingMaps(i)(argHead1.sentence.document)
        for (clusterPair <- stacked.protPair2Cluster.get(arg1 -> arg2))
          pairFeatures("boundWithout " + i + ": " + clusterPair.boundWithout(arg1, arg2)) = 1.0
      }
    }

    pairFeatures


    //relative position to trigger
    //dependency path between them
    //argument types/words: PROT, non-Prot, word
  }


  def argPairFeatures(arg1: RelationMentionArgument, arg2: RelationMentionArgument): Iterable[(Any, Double)] = {
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

  def argPairFeaturesForParents(arg1: RelationMentionArgument, arg2: RelationMentionArgument): Iterable[(Any, Double)] = {
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

  val useAggressiveNormalization = Conf.get("aggressivelyNormalize", false)

  val singularPronouns = Set("it", "its")
  val pluralPronouns = Set("they", "them", "their")
  val allPronouns = singularPronouns ++ pluralPronouns

  def vertexWalk(prefix: String, argFeatures: HashMap[String, Double], path: DependencyPath): Unit = {
    for (i <- 0 until path.size + 1) {
      val token = if (path.size > 0)
        stemTagHideProteins(if (i == path.size) path(i - 1).to else path(i).from)
      else "NoToken"
      val left = if (i == 0) "START" else path(i - 1).edgeString(true)
      val right = if (i == path.size) "END" else path(i).edgeString(true)
      argFeatures(prefix + "vw: %s %s %s".format(left, token, right)) = 1.0
      argFeatures(prefix + "vw: %s %s %s".format("*", token, right)) = 1.0
      argFeatures(prefix + "vw: %s %s %s".format(left, token, "*")) = 1.0
      argFeatures(prefix + "vw: %s %s %s".format("*", token, "*")) = 1.0
    }
  }

  def edgeWalk(prefix: String, argFeatures: HashMap[String, Double], path: DependencyPath): Unit = {
    for (edge <- path) {
      val left = stemTagHideProteins(edge.from)
      val right = stemTagHideProteins(edge.to)
      val middle = edge.edgeString(true)
      argFeatures(prefix + "ew: %s %s %s".format(left, middle, right)) = 1.0
      argFeatures(prefix + "ew: %s %s %s".format("*", middle, right)) = 1.0
      argFeatures(prefix + "ew: %s %s %s".format(left, middle, "*")) = 1.0
      argFeatures(prefix + "ew: %s %s %s".format(left, "*", right)) = 1.0
    }
  }

  def argFeatures(arg: RelationMentionArgument): Iterable[(Any, Double)] = {
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

      val dependencyStructure = argHead.sentence.getDependencyStructure.get
      val argHeads = dependencyStructure.heads(argHead)
      if (Conf.get("argHeadFeatures", true)) for (headEdge: DependencyStructure#Edge <- argHeads) {
        argFeatures(prefix + "arg heads: <-%s-%s".format(stemTagHideProteins(headEdge.head), headEdge.label)) = 1.0
        if (Conf.get("argSiblingFeatures", false))
          for (siblingEdge: DependencyStructure#Edge <- dependencyStructure.modifiers(headEdge.head);
               if (siblingEdge != headEdge)) {
            argFeatures(prefix + "arg sibling: <--%s->%s ".format(siblingEdge.label, stemTagHideProteins(siblingEdge.mod))) = 1.0
            argFeatures(prefix + "arg sibling: <-%s-%s-%s->%s".format(
              headEdge.label, stemTagHideProteins(headEdge.head), siblingEdge.label, stemTagHideProteins(siblingEdge.mod))) = 1.0
          }
        if (Conf.get("argSiblingReporter", false))
          for (siblingEdge: DependencyStructure#Edge <- dependencyStructure.modifiers(headEdge.head);
               if (siblingEdge != headEdge)) {
            if (siblingEdge.mod.word.toLowerCase.startsWith("reporter")) {
              argFeatures(prefix + "arg sibling: <--%s->%s ".format(siblingEdge.label, stemTagHideProteins(siblingEdge.mod))) = 1.0
              argFeatures(prefix + "arg sibling: <-%s-%s-%s->%s".format(
                headEdge.label, stemTagHideProteins(headEdge.head), siblingEdge.label, stemTagHideProteins(siblingEdge.mod))) = 1.0
            }
          }

      }


      if (Conf.get("closeDistanceFeatures", false)) for (feature <- closeDistanceFeatures(arg)) {
        argFeatures(prefix + feature) = 1.0
      }

      for (depName <- triggerHead.sentence.getDependencyStructureNames) {
        val structure = triggerHead.sentence.getDependencyStructure(depName)
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
          argFeatures(prefix + feature) = 1.0
        }
        argFeatures(prefix + "path length: " + path.length) = 1.0

        if (useAggressiveNormalization) {
          val aggPath = aggressiveNormalizePath(unnormalizedPath)
          val aggFeats = pathFeatures(aggPath)
          for (feature <- aggFeats) {
            argFeatures(prefix + "aggr: " + feature) = 1.0
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
              argFeatures(prefix + "coref (argHead:%s):%s".format(headEdge.label, pathFeat)) = 1.0
              argFeatures(prefix + "coref (argHead:%s,arg:%s):%s".format(headEdge.label, target, pathFeat)) = 1.0
              argFeatures(prefix + "coref (argHead:%s,trigger:%s):%s".format(headEdge.label, triggerStem, pathFeat)) = 1.0

            }
          }
        }

        if (Conf.get("leftright", false)) {
          for (i <- 0 until path.size) {
            val left = new DependencyPath(path.take(i + 1))
            val right = new DependencyPath(path.takeRight(i + 1))
            argFeatures(prefix + "left%d:".format(i) + left.toString(true)) = 1.0
            argFeatures(prefix + "left%d-trigger:".format(i) + triggerStem + left.toString(true)) = 1.0
            argFeatures(prefix + "right%d:".format(i) + right.toString(true)) = 1.0
            argFeatures(prefix + "right%d-lex:".format(i) + right.toString(true) + right.last.from.word.toLowerCase) = 1.0
          }
        }

        if (Conf.get("vertexWalk", false)) {
          vertexWalk(prefix, argFeatures, path)
        }
        if (Conf.get("edgeWalk", false)) {
          edgeWalk(prefix, argFeatures, path)
        }


      }
      if (Conf.get("stackedDiscourse", false) && arg.argIsEntityMention) {
        val mention = arg.entityMention
        val stackedFeats = new HashMap[String, Double]
        def addArgDiscourseFeats(i: Int, label: (String, String, String),
                                 discoursePrefix: String = "discourse ", statIndex: Int = 100000000): Unit = {
          val stackPrefix = prefix + discoursePrefix + i + ":" + label._1 + label._2
          val stackPrefix2 = prefix + discoursePrefix + i + ":" + label._1
          val stackPrefix3 = prefix + discoursePrefix + i
          stackedFeats(stackPrefix) = 1.0
          stackedFeats(stackPrefix2) = 1.0
          stackedFeats(stackPrefix3) = 1.0
          stackedFeats(prefix + label._1 + label._2 + "-sh " + (triggerStem.get == label._3)) = 1.0
          stackedFeats(prefix + label._1 + "-sh " + (triggerStem.get == label._3)) = 1.0
          if (statIndex < 2) {
            stackedFeats(stackPrefix + "-" + statIndex) = 1.0
          }

        }
        for (i <- 0 until doc2stackingMaps.size) {
          val stacked = doc2stackingMaps(i)(sentence.document)
          val (index, cluster) = stacked.protein2Cluster(mention)
          val stats = cluster.zippedStatsWithout(index)
          for ((stat, statIndex) <- stats; label <- stat; if (label._1 != BioNLPConstants.None)) {
            addArgDiscourseFeats(i, label, statIndex = statIndex)
          }
          val markovStats = cluster.previousStats(index)
          for (stat <- markovStats; label <- stat; if (label._1 != BioNLPConstants.None)) {
            addArgDiscourseFeats(i, label, "discourse-markov")
          }

        }
        if (Conf.get("conjoinDiscourse", false)) {
          val structure = triggerHead.sentence.getDependencyStructure.get
          val paths: ShortestPaths = structure.shortestPaths()
          //val path = aggressiveNormalizePath(paths.getPath(triggerHead, argHead))
          val unnormalizedPath = paths.getPath(triggerHead, argHead)
          val path = normalizePath(unnormalizedPath, structure)
          val pathRepr = path.toString(t => "", label => normalizeDepLabel(label))

          for (feat <- Seq("", triggerStem, pathRepr); (thatFeat, thatValue) <- stackedFeats) {
            argFeatures(thatFeat + " " + feat) = thatValue
          }
        } else {
          argFeatures ++= stackedFeats
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

  var currentDoc: Document = null
  val tokenHasProtein = new HashMap[Token, Boolean]
  val tokenHasProteinMemo = MemoUtil.concurrentHashMapMemo[Token, Boolean] {
    token =>
      token.sentence.entityMentionCandidates.exists(m =>
        m.begin.indexInDocument <= token.indexInDocument &&
          m.end.indexInDocument >= token.indexInDocument)
  }

  def isProteinToken(token: Token) = {
    val parallel = Conf.get("parallelFolds", false)
    if (!parallel && token.document != currentDoc) {
      currentDoc = token.document
      tokenHasProtein.clear
    }
    if (!parallel)
      tokenHasProtein.getOrElseUpdate(token, {
        token.sentence.entityMentionCandidates.exists(m =>
          m.begin.indexInDocument <= token.indexInDocument &&
            m.end.indexInDocument >= token.indexInDocument)
      })
    else {
      tokenHasProteinMemo(token)
    }
  }

  def annotate(doc: Document) = {
    proteinTokens.clear
    for (protein <- doc.allEntityMentionCandidates; token <- protein.span) {
      proteinTokens += token
    }
    val relMentions = doc.allRelationMentionCandidates
    for (relMention: RelationMention <- relMentions) {
      //relMention.features = Some(triggerFeatures(relMention))
      for (arg <- relMention.argumentCandidates) {
        //arg.features = Some(argFeatures(arg))
      }
    }
    logger.info("Extracted features for doc " + doc.id)
  }
}

object RelationMentionFeatureExtractor extends RelationMentionFeatureExtractor with CompactFeatureExtractor {
}

trait CompactFeatureExtractor extends RelationMentionFeatureExtractor with HasLogger {
  val makeFeaturesCompact = Conf.get("makeFeaturesCompact", false)

  val cutoff = Conf.get("featureCutoff", 0)
  class Counts extends HashMap[Any, Int] {
    override def default(key: Any) = 0
    def incr(key: Any) {
      this(key) = this(key) + 1
    }
  }
  val triggerCounts = new Counts
  val argCounts = new Counts
  val pairCounts = new Counts

  def stats(): String = {
    val totalTriggerFeats = triggerCounts.size
    val keptTriggerFeats = triggerCounts.filter(_._2 > cutoff).size
    val totalArgFeatures = argCounts.size
    val keptArgFeats = argCounts.filter(_._2 > cutoff).size
    val result = """
    |Cutoff:                               %d
    |Trigger Features  (kept/total/ratio): %d / %d / %4.3f
    |Argument Features (kept/total/ratio): %d / %d / %4.3f
    """.format(cutoff, keptTriggerFeats, totalTriggerFeats, keptTriggerFeats.toDouble / totalTriggerFeats,
      keptArgFeats, totalArgFeatures, keptArgFeats.toDouble / totalArgFeatures).stripMargin
    result
  }

  def count(docs: Iterable[Document]) {
    val counting = new Counting(50, count => logger.info("Counted features in %d docs".format(count)))
    for (doc <- counting(docs)) count(doc)
  }

  def count(doc: Document) {
    for (event <- doc.allRelationMentionCandidates) {
      for ((feat, value) <- super.triggerFeatures(event)) {
        triggerCounts.incr(feat)
      }
      for (arg <- event.argumentCandidates) {
        for ((feat, value) <- super.argFeatures(arg)) {
          argCounts.incr(feat)
        }
      }
    }
  }

  def cutOffTriggerFeats(pairs: Iterable[(Any, Double)]) = {
    if (cutoff == 0) pairs
    else {
      pairs.filter(pair => triggerCounts(pair._1) > cutoff)
    }
  }

  def cutOffArgFeats(pairs: Iterable[(Any, Double)]) = {
    if (cutoff == 0) pairs
    else {
      pairs.filter(pair => argCounts(pair._1) > cutoff)
    }
  }


  def compactify(pairs: Iterable[(Any, Double)]): Iterable[(Any, Double)] = {
    if (!makeFeaturesCompact) {
      pairs
    }
    else {
      pairs.map({
        case (key, value) => {
          val mapped = key match {
            case s: String => Symbol(s)
            case (s1: String, s2: String) => Symbol(s1) -> Symbol(s2)
            case s => s
          }
          mapped -> value
        }
      })
    }
  }
  override def argFeatures(arg: RelationMentionArgument) = {
    compactify(cutOffArgFeats(super.argFeatures(arg)))
  }
  override def triggerFeatures(relMention: RelationMention) = {
    compactify(cutOffTriggerFeats(super.triggerFeatures(relMention)))
  }
  override def protPairFeatures(arg1: EntityMention, arg2: EntityMention) = {
    compactify(super.protPairFeatures(arg1, arg2))
  }
}


