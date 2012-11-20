package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.data._
import java.io._
import io.Source
import collection.mutable.{HashSet, ArrayBuffer, HashMap}
import cc.refectorie.proj.factorieie.util.{WhatsWrongOutputGenerator, HasLogger}
import java.util.NoSuchElementException

/**
 * @author sriedel
 */

class BioNLPEvaluator(val evalScriptDir: File, val resultLog: String = "eval.out") extends HasLogger {

  import BioNLPConstants._

  val doTask2 = Conf.get("doTask2", false)
  val runA2Evaluate = Conf.get("runA2Evaluate", true)
  val runA2Normalize = Conf.get("runA2Normalize", true)

  sealed trait BioNLPEntity
  case class Protein(mention: EntityMention) extends BioNLPEntity
  case class Arg(role: String, arg: BioNLPEntity)
  case class Event(label: String, mention: RelationMention, args: Seq[Arg]) extends BioNLPEntity
  case class ArgGroup(args: Seq[Arg], collective: Boolean = false)

  case class EvalOutputRow(eventClass: String,
                           gold: Int, goldMatch: Int,
                           answer: Int, answerGold: Int,
                           recall: Double, prec: Double, fscore: Double) {
    override def toString = "%-30s %4d/%-4d %4d/%-4d %-6.3f %-6.3f %-6.3f".format(
      eventClass, goldMatch, gold, answerGold, answer, recall, prec, fscore)
  }

  class EvalOutput {
    val rows = new ArrayBuffer[EvalOutputRow]
    val table = new ArrayBuffer[String]
  }


  def evaluate(docs: Iterable[Document], state: State, goldDir: File, targetDir: File): EvalOutput = {
    if (!targetDir.exists) {
      targetDir.mkdirs
      logger.info("Created target dir %s".format(targetDir.getAbsolutePath))
    }
    val a2FileNames = new ArrayBuffer[String]
    for (doc <- docs) {
      val fileId = doc.id.substring(doc.id.lastIndexOf('/') + 1, doc.id.lastIndexOf('.'))
      val a2Out = new File(targetDir, fileId + ".a2")
      val events = for (sentence <- doc.sentences; event <- stateToBioNLPEvents(state, sentence)) yield event
      val outputStream = new FileOutputStream(a2Out)
      printA2File(outputStream, events)
      a2FileNames += a2Out.getAbsolutePath
      val a2GoldFile = new File(goldDir, fileId + ".a2")
      outputStream.close
      //now copy a1 and a2 and txt file from gold directory to simplify comparison
      copyFile(new File(goldDir, fileId + ".a1"), new File(targetDir, fileId + ".a1.gold"))
      if (a2GoldFile.exists) copyFile(a2GoldFile, new File(targetDir, fileId + ".a2.gold"))
      copyFile(new File(goldDir, fileId + ".txt"), new File(targetDir, fileId + ".txt"))

    }
    logger.info("Created %d a2 files in %s".format(a2FileNames.size, targetDir.getAbsolutePath))
    val normalizeScript = new File(evalScriptDir, "a2-normalize.pl").getAbsolutePath
    val evalScript = new File(evalScriptDir, "a2-evaluate.pl").getAbsolutePath

    val normalizeCMD = "%s -g %s -u %s".format(normalizeScript, goldDir.getAbsolutePath, a2FileNames.mkString(" "))
    var normalize: Process = null
    if (runA2Normalize) {
      normalize = Runtime.getRuntime().exec(normalizeCMD)
      for (line <- Source.fromInputStream(normalize.getErrorStream).getLines) {
        logger.warn("[a2-normalize]:" + line)
      }
      logger.info("Run normalization in place".format(a2FileNames.size, targetDir.getAbsolutePath))
    }


    var evaluate: Process = null
    val result = new EvalOutput

    if (runA2Evaluate) {
      val taskCommand = if (doTask2) "-t 2" else "-t 1"
      val evalCMD = "%s -g %s %s -p -s -x -v %s".format(evalScript, goldDir.getAbsolutePath,
        taskCommand, a2FileNames.mkString(" "))
      evaluate = Runtime.getRuntime.exec(evalCMD)
      val tempOut = new PrintStream(new FileOutputStream(resultLog, true))
      tempOut.println(Conf.properties)
      val contentAfterDashes = Seq(false, false, true, true, true, false, false)
      var dashCount = 0
      for (line <- Source.fromInputStream(evaluate.getInputStream).getLines) {
        tempOut.println(line)
        if (line.startsWith("-------")) {
          println(line)
          dashCount += 1
          result.table += line
        } else if (contentAfterDashes(dashCount)) {
          println(line)
          val split = line.trim.split("[\\s\\(\\)]+")
          val Array(ec, gold, gMatch, a, aMatch, r, p, f) = split
          result.table += line
          result.rows +=
            EvalOutputRow(ec, gold.toInt, gMatch.toInt, a.toInt, aMatch.toInt, r.toDouble, p.toDouble, f.toDouble)
        }
      }
    }
    if (runA2Normalize) {
      normalize.getOutputStream.close
      normalize.getInputStream.close
      normalize.getErrorStream.close
    }
    if (runA2Evaluate) {
      evaluate.getInputStream.close
      evaluate.getOutputStream.close
    }
    result
  }

  def copyFile(from: File, to: File) {
    val toStream = new FileOutputStream(to)
    val fromStream = new FileInputStream(from)

    toStream.getChannel().transferFrom(fromStream.getChannel, 0, Long.MaxValue)

    toStream.close
    fromStream.close
  }

  def resolve(mention: SentenceMention[_], state: State,
              visited: Set[SentenceMention[_]] = Set.empty): Seq[BioNLPEntity] = {
    if (visited(mention)) {
      logger.warn("Found loop at %s in doc %s".format(mention, mention.head.document.id))
      return Seq.empty
    }
    val result = mention match {
      case m: EntityMention => Seq(Protein(m))
      case m: RelationMention => {
        val label = state.getValue(m.label)
        if (label.isDefined && label.get != None) {
          val groups = groupArgs(m, state, visited ++ Set(m))
          val expanded = expand(groups)
          for (args <- expanded) yield Event(label.get.toString, m, args)
        }
        else Seq.empty
      }
      case _ => error("Not supported.")
    }
    result

  }

  var groupBindingBySyntax = true
  var groupBindingByPrediction = Conf.get("groupByPrediction", false)
  var nnID = 0

  private def pathReprForClustering(path: DependencyPath): Option[String] = {
    for (edge <- path.headOption) yield {
      "%s/%d%s".format(
        edge.edgeString(true),
        path.length,
        if (edge.edge.label == "nn") {
          nnID += 1;
          nnID.toString
        } else "")
    }
  }

  val associativeTriggers = Set("interaction", "heterodimer", "interactions", "heterotetramer", "association", "form",
    "-binding", "associate", "complex", "complexes", "associations")
  val equiDependencies = Set("prep_including", "prep_like", "appos", "dep")

  def sameGoldBinding(arg1: RelationMentionArgument, arg2: RelationMentionArgument): Boolean = {
    arg1.relationIDs.exists(id => arg2.relationIDs(id)) &&
      arg1.entityMention.entityKey != arg2.entityMention.entityKey
  }

  def groupable(arg1: RelationMentionArgument, arg2: RelationMentionArgument, state: State): Boolean = {

    if (doTask2 && (arg1.argIsEntityMention && arg1.entityMention.tags(EntityTag) ||
      arg2.argIsEntityMention && arg2.entityMention.tags(EntityTag))) {
      return arg1.argIsEntityMention && arg1.entityMention.tags(EntityTag) &&
        arg2.argIsEntityMention && arg2.entityMention.tags(EntityTag)
    }
    if (groupBindingByPrediction) {
      val (left, right) = if (arg1.entityMention.head.indexInDocument < arg2.entityMention.head.indexInDocument)
        (arg1.entityMention, arg2.entityMention)
      else (arg2.entityMention, arg1.entityMention)
      println("*******")
      try {
        val guess = state(left -> right)
        println(left.phrase + "-" + right.phrase + ": " + guess + "/" + sameGoldBinding(arg1, arg2))
        if (left.begin.sentence == right.end.sentence)
          println(arg1.arg.head.sentence.markedSpans(Seq(arg1.arg, arg2.arg, arg1.owner)))
        println("%s %s %s %s".format(arg1.relationIDs, arg1.entityMention.entityKey, arg2.relationIDs, arg2.entityMention.entityKey))
        if (guess != sameGoldBinding(arg1, arg2)) {
          for ((feat, weight) <- RelationMentionFeatureExtractor.protPairFeatures(arg1.entityMention, arg2.entityMention)) {
            println("   " + feat)
          }
        }
        !guess.asInstanceOf[Boolean]
      } catch {
        case _ => {
          logger.warn("No binding state predicted for " + (left -> right))
          false
        }
      }
    } else {
      val relMention = arg1.owner
      val (left, right) = if (arg1.arg.head.indexInDocument < arg2.arg.head.indexInDocument) (arg1, arg2) else (arg2, arg1)
      val paths: ShortestPaths = relMention.sentence.getDependencyStructure("mcclosky").shortestPaths()
      val path1 = paths.getPath(relMention.head, left.arg.head)
      val path2 = paths.getPath(relMention.head, right.arg.head)
      for (lastEdge1 <- path1.lastOption; lastEdge2 <- path2.lastOption) {
        if (path1.length == path2.length && lastEdge1.edge.label != "nn"
          && lastEdge1.edgeString(true) == lastEdge2.edgeString(true)) return true
      }
      if (right.arg.begin.word.startsWith("/") && left.arg.end.nextOption == Some(right.arg.begin)) return true
      val path = paths.getPath(left.arg.head, right.arg.head)
      if (path.length == 1 && path(0).edge.label.startsWith("conj")) return true

      //PROTA or other X including PROTB and PROTC
      if (path.length == 2 && path(0).edge.label.startsWith("conj") && equiDependencies(path(1).edge.label)) return true

      //for PROT A 1, 2, 3 and 4.
      if (path.length == 1 && path(0).edge.label == "num") return true
      if (path.length == 2 && path(0).edge.label.startsWith("num") && path(1).edge.label.startsWith("num")) return true

      //PROTA and PROTB/PROTC
      if (path.length == 2 && path(0).edge.label.startsWith("conj") && path(0).to.word.startsWith("/")) return true
      false
    }
  }

  def groupBinding(relMention: RelationMention, state: State, visited: Set[SentenceMention[_]]): Seq[ArgGroup] = {

    //todo: group via dependencies between args (conj_
    def checkRole(role: String) = role != None && role != NoSite
    val activeArgs = relMention.argumentCandidates.filter(arg => checkRole(state(arg.role).toString))
    val paths: ShortestPaths = relMention.sentence.getDependencyStructure("mcclosky").shortestPaths()
    val grouped = if (groupBindingBySyntax || groupBindingByPrediction) {
      val groups = new ArrayBuffer[ArrayBuffer[RelationMentionArgument]]
      for (arg <- activeArgs) {
        var found = false
        for (group <- groups; if (!found)) {
          for (other <- group; if (!found)) {
            if (groupable(arg, other, state)) {
              found = true
              group += arg
            }
          }
        }
        if (!found) {
          val group = new ArrayBuffer[RelationMentionArgument]
          group += arg
          groups += group
        }
      }
      if (!groupBindingByPrediction && activeArgs.size >= 2 && associativeTriggers(relMention.head.word.toLowerCase)) {
        val path = paths.getPath(activeArgs(0).arg.head, activeArgs(1).arg.head)
        val result = if (path.length == 1 && path(0).edge.label == "conj_or") groups else activeArgs.map(Seq(_))
        result
      } else
        groups
    } else {
      activeArgs.map(Seq(_))
    }
    for (group <- grouped) yield {
      val args = for (arg <- group) yield Arg(state(arg.role).toString, resolve(arg.arg, state, visited).head)
      ArgGroup(args, false) //true?
    }
  }

  def groupArgs(relMention: RelationMention, state: State,
                visited: Set[SentenceMention[_]]): Seq[ArgGroup] = {
    def checkRole(role: String) = role != None && role != NoSite
    state(relMention.label) match {
      case Binding =>
        if (groupBindingBySyntax || groupBindingByPrediction) groupBinding(relMention, state, visited)
        else {
          val args = for (arg <- relMention.argumentCandidates; if (checkRole(state(arg.role).toString))) yield
            Arg(state(arg.role).toString, resolve(arg.arg, state, visited).head)
          Seq(ArgGroup(args, true))
        }
      case rel => {
        //group by role
        val roles = relMention.argumentCandidates.filter(a => checkRole(state(a.role).toString)).map(a => state(a.role)).toSet.toSeq
        for (role <- roles; if (role != None)) yield {
          val args = relMention.argumentCandidates.filter(arg => state(arg.role) == role)
          val argEntities = args.flatMap(a => resolve(a.arg, state, visited))
          ArgGroup(argEntities.map(Arg(role.toString, _)), false)
        }
      }
    }
  }

  def expand(groups: Seq[ArgGroup]): Seq[Seq[Arg]] = {
    if (groups.isEmpty) Seq(Seq.empty)
    else groups.head match {
      case ArgGroup(args, true) => for (tail <- expand(groups.drop(1))) yield args ++ tail
      case ArgGroup(args, false) => for (head <- args; tail <- expand(groups.drop(1))) yield Seq(head) ++ tail
    }
  }

  def removeEvent(state: MutableState, relMention: RelationMention) {
    if (state(relMention.label) != None) {
      state(relMention.label) = None
      for (argIn <- relMention.argumentInCandidates) {
        state(argIn.role) = None
        consistify(state, argIn.owner)
      }
    }
  }

  def removeIncomingEdgesForNonEvents(state: MutableState, relMention: RelationMention) = {
    if (state(relMention.label) == None) {
      for (incoming <- relMention.containedInCandidates) {
        if (state(incoming.value.role) != None) {
          logger.warn("Incoming edge to non-event token %s in document %s".format(relMention, relMention.head.document.id))
          state(incoming.value.role) = None
        }
      }
    }
  }

  def consistify(state: MutableState, relMention: RelationMention): Boolean = {
    val label = state(relMention.label)
    if (label == None || label == EventSpecs.Process) true
    else {
      val themeExists = relMention.argumentCandidates.exists(arg => state(arg.role) == Theme)
      if (!themeExists) {
        logger.warn("Missing theme for event %s in document %s".format(relMention, relMention.head.document.id))
        removeEvent(state, relMention)
        false
      } else true
    }
    true
  }
  def fixSelfRegulation(state: MutableState, relMention: RelationMention): Boolean = {
    val label = state(relMention.label).toString
    if (label == None || !EventSpecs.regulations(label)) true
    else {
      val selfThemeExists = relMention.argumentInCandidates.exists(arg =>
        state(arg.role) == Theme && arg.owner.head == arg.arg.head)
      if (selfThemeExists) {
        state(relMention.label) = GeneExpression
        logger.warn("Self regulation fixed in doc %s".format(relMention.sentence.document.id))
        false
      }
      else true
    }
  }

  def fixTask2Roles(state: MutableState, relMention: RelationMention) {
    val label = state(relMention.label).toString
    if (label == None || !EventSpecs.regulations(label)) return
    val causeExists = relMention.argumentCandidates.exists(arg => state(arg.role) == Cause)
    if (!causeExists) {
      for (arg <- relMention.argumentCandidates;
           if (arg.argIsEntityMention && arg.entityMention.tags(EntityTag))) {
        if (state(arg.role) == "CSite") {
          state(arg.role) = NoSite
          logger.warn("Task 2 CSite fixed in doc %s".format(relMention.sentence.document.id))
        }
      }
    }
  }


  def fixNontransitivityIn(state: MutableState, relMention: RelationMention): Unit = {
    if (!state(relMention.label).toString.endsWith("egulation")) return
    val proteinArgsOfChildren = new HashSet[(EntityMention, String)]

    for (arg <- relMention.argumentCandidates; if (isArg(state(arg.role)) && arg.argIsRelationMention)) {
      for (protArg <- arg.relationMention.argumentCandidates; if (isArg(state(protArg.role)) && protArg.argIsEntityMention))
        proteinArgsOfChildren += protArg.entityMention -> state(protArg.role).toString
    }
    for (arg <- relMention.argumentCandidates; if (isArg(state(arg.role)) && arg.argIsEntityMention)) {
      if (proteinArgsOfChildren(arg.entityMention -> state(arg.role).toString)) {
        state(arg.role) = None
        logger.info("Fixed non-transitivity")

      }
    }
  }

  val ignoreRoleForNonTransitivityFix = Conf.get("ignoreRoleForNonTransitivityFix", false)

  def isArgRole(value: String): Boolean = value != None && value != NoSite
  def isArg(value: Any): Boolean = isArgRole(value.toString)

  def fixNontransitivityForEventsAndProteinsIn(state: MutableState, relMention: RelationMention): Unit = {

    val roleRepr: String => String = if (ignoreRoleForNonTransitivityFix) s => "" else identity(_)

    if (!state(relMention.label).toString.endsWith("egulation")) return
    val argsOfChildren = new HashSet[(Any, String)]
    for (arg <- relMention.argumentCandidates;
         if (isArg(state(arg.role)) && arg.argIsRelationMention && state(arg.relationMention.label) != None)) {
      for (protArg <- arg.relationMention.argumentCandidates; if (isArg(state(protArg.role))))
        argsOfChildren += protArg.arg.asInstanceOf[Any] -> roleRepr(state(protArg.role).toString)
    }
    for (arg <- relMention.argumentCandidates; if (isArg(state(arg.role)))) {
      if (argsOfChildren(arg.arg.asInstanceOf[Any] -> roleRepr(state(arg.role).toString))) {
        val themeCount = relMention.argumentCandidates.filter(arg => state(arg.role) == Theme).size
        if (themeCount < 2 && state(arg.role) == Theme) {
          logger.warn("Theme count would be 0 if nontransitivity was fixed in doc %s"
            .format(relMention.sentence.document.id))
          //          logger.warn("Current arguments: %s".format(
          //            relMention.argumentCandidates.filter(arg => state(arg.role) != None).map(arg => arg -> state(arg.role)).mkString(",")))
          //          logger.warn("Current arguments of children: %s".format(argsOfChildren.mkString(",")))
        } else {
          logger.info("Fixed non-transitivity in %s event for role %s in doc %s".format(
            state(relMention.label).toString, state(arg.role).toString, relMention.sentence.document.id))
          state(arg.role) = None
        }
      }
    }

  }


  val fixNonTransitivity = Conf.get("fixNonTransitivity", false)
  val fixSelfRegulation = Conf.get("fixSelfRegulation", true)
  val fixTaskTwoRoles = Conf.get("fixTask2Roles", true)
  val removeDanglingEdges = Conf.get("removeDangling", false)
  val predictBindingPairs = Conf.get("predictBindingPairs", true)

  def stateToBioNLPEvents(state: State, sentence: Sentence): Set[Event] = {
    val clean = postprocess(state, sentence)
    sentence.relationMentionCandidates.flatMap(resolve(_, clean)).filter(_.isInstanceOf[Event]).
      map(_.asInstanceOf[Event]).toSet
  }

  def postprocess(state: State, sentence: Sentence): State = {
    val clean = new MutableState
    for (event <- sentence.relationMentionCandidates) {
      clean(event.label) = state.getValue(event.label).getOrElse(None)
      for (arg <- event.argumentCandidates) {
        clean(arg.role) = state.getValue(arg.role).getOrElse(None)
      }
    }
    if (removeDanglingEdges) for (event <- sentence.relationMentionCandidates) {
      removeIncomingEdgesForNonEvents(clean, event)
    }
    for (event <- sentence.relationMentionCandidates) {
      consistify(clean, event)
    }
    if (fixNonTransitivity) for (event <- sentence.relationMentionCandidates) {
      fixNontransitivityForEventsAndProteinsIn(clean, event)
    }
    if (fixSelfRegulation) for (event <- sentence.relationMentionCandidates) {
      fixSelfRegulation(clean, event)
    }

    if (doTask2 && fixTaskTwoRoles) for (event <- sentence.relationMentionCandidates){
      fixTask2Roles(clean, event)
    }

    if (predictBindingPairs) for (prot1 <- sentence.entityMentionCandidates;
                                  prot2 <- sentence.entityMentionCandidates;
                                  if (!prot1.tags(EntityTag) && !prot2.tags(EntityTag));
                                  if (prot1.head.indexInDocument < prot2.head.indexInDocument)) {
      try {
        clean(prot1 -> prot2) = state(prot1 -> prot2)
      } catch {
        case e: NoSuchElementException => {
          logger.warn("No protein binding state found for %s-%s. Setting it to false".format(prot1, prot2))
          clean(prot1 -> prot2) = false
        }
      }
    }
    clean
  }

  def printA2File(outputStream: OutputStream, events: Iterable[Event]) = {
    val out = new PrintStream(outputStream)
    val eventIDs = new HashMap[Event, String]
    val triggerIDs = new HashMap[RelationMention, String]
    val trigger2Type = new HashMap[RelationMention, String]

    val triggers = events.map(_.mention).toSet

    //assign event IDs, and remember trigger to type mapping
    for (event <- events) {
      eventIDs(event) = "E" + eventIDs.size
      trigger2Type(event.mention) = event.label
    }

    //print triggers and remember trigger IDs
    for (mention <- triggers) {
      val id = "T" + (triggerIDs.size + 1000)
      triggerIDs(mention) = id
      out.println("%s\t%s %d %d\t%s".format(id,
        trigger2Type(mention), mention.head.charOffsetBegin, mention.head.charOffsetEnd,
        mention.head.word))
    }

    if (doTask2) {
      val alreadyPrinted = new HashSet[String]
      for (event <- events) {
        for (arg <- event.args) {
          arg.arg match {
            case Protein(mention) if (mention.tags(EntityTag) && !alreadyPrinted(mention.entityKey.get)) => {
              out.println("%s\t%s %d %d\t%s".format(mention.entityKey.get,
                "Entity", mention.head.charOffsetBegin, mention.head.charOffsetEnd,
                mention.head.word))
              alreadyPrinted += mention.entityKey.get
            }
            case _ => {}
          }
        }
      }
    }

    //now print out events and arguments
    for (event <- events) {
      val eventID = eventIDs(event)
      val triggerID = triggerIDs(event.mention)
      out.print(eventID + "\t")
      out.print("%s:%s ".format(event.label, triggerID))
      val counts = new HashMap[String, Int] {
        override def default(key: String) = 0
      }
      def roleNormalize(role: String): String = {
        val result = counts(role) match {
        //case 0 => role
          case x => role + (x + 1).toString
        }
        counts(role) = counts(role) + 1
        result
      }
      val args = for (arg <- event.args) yield {
        val role = if (!Conf.get("numberRoles", false) || event.label != Binding) arg.role else roleNormalize(arg.role)
        arg.arg match {
          case Protein(m) => "%s:%s".format(role, m.entityKey.get)
          case e@Event(_, m, _) => "%s:%s".format(role, eventIDs(e))
        }
      }
      out.println(args.mkString(" "))
    }
  }

  def createTempDirectory: File = {
    val temp = File.createTempFile("bionlp", "eval")
    if (!temp.delete) throw new IOException("Could not delete temp file: " + temp.getAbsolutePath)
    if (!temp.mkdir) throw new IOException("Could create temp dir: " + temp.getAbsolutePath)
    logger.debug("Created Temp Dir ".format(temp.getAbsolutePath))
    temp
  }
}


object BioNLPEvaluator {
  def main(args: Array[String]) {
    //    DB.dropAll
    //    DB.copyFrom(AnnotatedDB)

    //    val docs = AnnotatedKB.documents.filter(_.id.endsWith("7492771.txt"))
    val dataset = args(0)

    val docs = AnnotatedKB.documents.query(Tag(dataset))

    val evaluator = new BioNLPEvaluator(Conf.get[File]("evalScriptDir"), "/tmp/roundtrip.out")

    val goldDir = Conf.get[File](dataset)

    evaluator.evaluate(docs, TrueValueState, goldDir, new File("/tmp/eval-%s.out".format(dataset)))

    val ww = new PrintStream("/tmp/eval-%s.ww".format(dataset))

    for (doc <- docs) {
      ww.println(WhatsWrongOutputGenerator.toWhatsWrong(doc, true))
    }
    ww.close

  }
}