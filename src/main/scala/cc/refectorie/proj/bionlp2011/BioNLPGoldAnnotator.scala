package cc.refectorie.proj.bionlp2011

import java.io.File
import cc.refectorie.proj.factorieie.annotator.Annotator
import io.Source
import cc.refectorie.proj.factorieie.util.{Util, HasLogger}
import cc.factorie._
import BioNLPUtils._
import collection.mutable.{HashSet, HashMap}
import cc.refectorie.proj.factorieie.data._
import BioNLPConstants._


/**
 * Loads BioNLP a2 files and sets truth values for every relation mention and
 * argument that can be matched to the gold data.
 *
 * @author sriedel
 */
class BioNLPGoldAnnotator(dir: File) extends Annotator with HasLogger {
  def normalizeRole(role: String) = {
    if (role.last.isDigit) role.substring(0, role.length - 1) else role
  }

  val copyEqui = Conf.get("copyEqui", false)

  def annotate(a1file: File, a2file: File, doc: Document) {
    val id2offsets = new HashMap[String, (Int, Int)]

    //remember where the entities are
    for (line <- Source.fromFile(a1file).getLines) {
      val Array(id, labelAndOffsets, text) = line.split("\t")
      val Array(label, begin, end) = labelAndOffsets.split(" ")
      id2offsets(id) = (begin.toInt, end.toInt)
    }
    //remember where the triggers are
    val a2source = Source.fromFile(a2file).getLines.toArray
    for (line <- a2source; if (line.startsWith("T"))) {
      val Array(id, labelAndOffsets, text) = line.split("\t")
      val Array(label, begin, end) = labelAndOffsets.split(" ")
      id2offsets(id) = (begin.toInt, end.toInt)
    }

    //set gold entities
    if (doTask2)
      for (line <- a2source; if (line.startsWith("T"))) {
        val Array(id, labelAndOffsets, text) = line.split("\t")
        val Array(label, begin, end) = labelAndOffsets.split(" ")
        if (label == "Entity") {
          val headToken = doc.tokenAt(end.toInt - 1).get
          for (entityMention <- headToken.sentence.entityMentionCandidates.filter(_.tags(EntityTag))) {
            entityMention.entityType.trueValue = Entity
            entityMention.exists.trueValue = true
          }
        }
      }


    //remember event triggers
    for (line <- a2source; if (line.startsWith("E"))) {
      val Array(id, typeAndTrigger, rest@_*) = line.split("\\s+")
      val Array(eventType, triggerId) = typeAndTrigger.split(":")
      id2offsets(id) = id2offsets(triggerId)
    }

    //use antecedents (before parentheses) instead of subsequent protein mentions
    //remember subsequent mentions
    val coref = new HashMap[(Int, Int), Seq[(Int, Int)]]
    for (line <- a2source; if (line.startsWith("*"))) {
      val Array(_, _, ids@_*) = line.split("\\s+")
      val leftMost = ids.map(id => id -> id2offsets(id)._1).maxByInt(-_._2)._1
      coref(id2offsets(leftMost)) = ids.filter(_ != leftMost).map(id2offsets(_))
      for (id <- ids; if (id != leftMost)) {
        id2offsets(id) = id2offsets(leftMost)
      }
    }


    //assign true values based on pred-arg pairs.
    for (line <- a2source; if (line.startsWith("E"))) {
      val Array(id, typeAndTrigger, rest@_*) = line.split("\\s+")
      val Array(eventType, triggerId) = typeAndTrigger.split(":")

      val (beginTrigger, endTrigger) = id2offsets(triggerId)
      val headTokenOption = doc.tokenAt(endTrigger - 1)
      if (!headTokenOption.isDefined) {
        println(line)
        println(doc.tokens.map(t => "%s (%d,%d)".format(t.word, t.charOffsetBegin, t.charOffsetEnd)).mkString(" "))
        error("token for trigger head can't be found")
      }

      val headToken = headTokenOption.get

      def isSelfMention(mention: SentenceMention[_]) = {
        mention match {
          case rm: RelationMention => rm.argumentCandidates.size == 1 &&
            rm.argumentCandidates.head.arg.head == rm.head
          case _ => false
        }
      }

      //find the rel mention candidate on this token that just points to itself
      val selfOption = headToken.sentence.relationMentionCandidates.find(m =>
        m.head == headToken && isSelfMention(m))

      //if this a2 line contains a pointer to an argument with the same offsets, it's a "self-regulation"
      val args = rest.map(_.split(":")(1))
      if (args.exists(argId => id2offsets(argId) == id2offsets(id))) {
        for (self <- selfOption) {
          self.label.trueValue = eventType
          self.exists.trueValue = true
          self.argumentCandidates.head.role.trueValue = "Theme"
          self.argumentCandidates.head.exists.trueValue = true
        }
      }
      else {
        //find the proper relation mention on this token
        val relationMentionOption = headToken.sentence.relationMentionCandidates.find(m =>
          m.head == headToken && selfOption.getOrElse(null) != m)
        for (relationMention <- relationMentionOption) {
          var themeMatched = false
          for (arg <- rest; if (arg.startsWith("Theme") || arg.startsWith("Cause") || arg.startsWith(Participant) || doTask2)) {
            val Array(role, argId) = arg.split(":")
            val (beginArg, endArg) = id2offsets(argId)
            val argHead = headFinder(doc.tokenAt(beginArg).get, doc.tokenAt(endArg - 1).get)
            //determine whether to use the self mention, or the proper mention
            val argHeads = new HashSet[Token]
            argHeads += argHead
            if (copyEqui) {
              for (others <- coref.get(beginArg -> endArg); (b, e) <- others) {
                val head = headFinder(doc.tokenAt(b).get, doc.tokenAt(e - 1).get)
                argHeads += head
              }
            }
            val found = relationMention.argumentCandidates.filter(arg => argHeads(arg.arg.head))
            val activeSelf = found.find(arg => isSelfMention(arg.arg) && arg.relationMention.exists.trueValue)
            val argMentions = activeSelf.map(Seq(_)).getOrElse(found)
            val antecedents = new HashSet[EntityMention]
            val anaphora = new HashSet[EntityMention]
            for (argMention <- argMentions) {
              //            if (!found.isEmpty) {
              //              val argMention = activeSelf.getOrElse(found.head)
              val normRole = normalizeRole(role)

              if (!doTask2 || EventSpecs.compatibleRoles(eventType, argMention)(normRole)) {
                argMention.role.trueValue = normRole
                argMention.exists.trueValue = true
                argMention.relationIDs = Set(id) ++ argMention.relationIDs
                themeMatched = normRole == "Theme" || themeMatched
                logger.trace("Matched argument %s".format(argId))
                if (argMention.argIsEntityMention && argMention.arg.head == argHead) {
                  antecedents += argMention.entityMention
                } else if (argMention.argIsEntityMention) {
                  anaphora += argMention.entityMention
                }
              }
            }
            //if there are any copied equi args, we make sure that they have the same entity ID.
            if (Conf.get("copyEquiIDs", true))
              for (ante <- antecedents; ana <- anaphora) {
                ana.entityKey = ante.entityKey
              }
            if (argMentions.isEmpty)
              logger.trace("Couldn't match argument %s of event %s".format(arg, id))

          }
          if (themeMatched || eventType == "Process") {
            //todo: do we really avoid these when learning?
            relationMention.label.trueValue = eventType
            relationMention.exists.trueValue = true
            logger.trace("Matched rel mention %s".format(id))
          } else {
            relationMention.label.truthKnown = false
          }

        }
      }
    }
  }
  def annotate(doc: Document): Unit = {
    val slashIndex = Util.optMinusOne(doc.id.lastIndexOf('/')).getOrElse(0)
    val filename = doc.id.substring(slashIndex)
    val a1file = new File(dir, filename.substring(0, filename.indexOf('.')) + ".a1")
    val a2file = new File(dir, filename.substring(0, filename.indexOf('.')) + ".a2")

    if (!a2file.exists) {
      logger.info("Gold file %s can't be found, no gold annotation possible".format(a2file.getAbsolutePath))
      return
    }

    annotate(a1file, a2file, doc)

    a1file.close
    a2file.close

    logger.info("Added gold information from a2 file " + a2file.getAbsolutePath)

  }

  val doTask2 = Conf.get("doTask2", false)

  def createMap(doc: Document): PredictionStats = {
    //hack: this doesn't work for task2 roles, so turn this off for now
    val doTask2 = false
    val result = new PredictionStats(doc)
    val slashIndex = Util.optMinusOne(doc.id.lastIndexOf('/')).getOrElse(0)
    val filename = doc.id.substring(slashIndex)
    val a1file = new File(dir, filename.substring(0, filename.indexOf('.')) + ".a1")
    val a2file = new File(dir, filename.substring(0, filename.indexOf('.')) + ".a2")

    if (!a2file.exists) {
      logger.info("Gold file %s can't be found, no gold annotation possible".format(a2file.getAbsolutePath))
      return result
    }

    val id2offsets = new HashMap[String, (Int, Int)]

    //remember where the entities are
    val a1Source = Source.fromFile(a1file)
    for (line <- a1Source.getLines) {
      val Array(id, labelAndOffsets, text) = line.split("\t")
      val Array(label, begin, end) = labelAndOffsets.split(" ")
      id2offsets(id) = (begin.toInt, end.toInt)
    }

    a1Source.close
    a1file.close

    //remember where the triggers are
    val a2Source = Source.fromFile(a2file)
    val a2Lines = a2Source.getLines.toArray
    for (line <- a2Lines; if (line.startsWith("T"))) {
      val Array(id, labelAndOffsets, text) = line.split("\t")
      val Array(label, begin, end) = labelAndOffsets.split(" ")
      if (label != "Entity") id2offsets(id) = (begin.toInt, end.toInt)
    }
    a2Source.close
    a2file.close

    //set gold entities
    if (doTask2)
      for (line <- a2Lines; if (line.startsWith("T"))) {
        val Array(id, labelAndOffsets, text) = line.split("\t")
        val Array(label, begin, end) = labelAndOffsets.split(" ")
        if (label == "Entity") {
          val headToken = doc.tokenAt(end.toInt - 1).get
          for (entityMention <- headToken.sentence.entityMentionCandidates.filter(_.tags(EntityTag))) {
            entityMention.entityType.trueValue = Entity
            entityMention.exists.trueValue = true
          }
        }
      }

    //remember event triggers
    for (line <- a2Lines; if (line.startsWith("E"))) {
      val Array(id, typeAndTrigger, rest@_*) = line.split("\\s+")
      val Array(eventType, triggerId) = typeAndTrigger.split(":")
      id2offsets(id) = id2offsets(triggerId)
    }

    //use antecedents (before parentheses) instead of subsequent protein mentions
    //remember subsequent mentions
    val coref = new HashMap[(Int, Int), Seq[(Int, Int)]]
    for (line <- a2Lines; if (line.startsWith("*"))) {
      val Array(_, _, ids@_*) = line.split("\\s+")
      val leftMost = ids.map(id => id -> id2offsets(id)._1).maxByInt(-_._2)._1
      coref(id2offsets(leftMost)) = ids.filter(_ != leftMost).map(id2offsets(_))
      for (id <- ids; if (id != leftMost)) {
        id2offsets(id) = id2offsets(leftMost)
      }
    }


    //assign true values based on pred-arg pairs.
    for (line <- a2Lines; if (line.startsWith("E"))) {
      val Array(id, typeAndTrigger, rest@_*) = line.split("\\s+")
      val Array(eventType, triggerId) = typeAndTrigger.split(":")

      val (beginTrigger, endTrigger) = id2offsets(triggerId)
      val headToken = doc.tokenAt(endTrigger - 1).get

      def isSelfMention(mention: SentenceMention[_]) = {
        mention match {
          case rm: RelationMention => rm.argumentCandidates.size == 1 &&
            rm.argumentCandidates.head.arg.head == rm.head
          case _ => false
        }
      }

      //find the rel mention candidate on this token that just points to itself
      val selfOption = headToken.sentence.relationMentionCandidates.find(m =>
        m.head == headToken && isSelfMention(m))

      //if this a2 line contains a pointer to an argument with the same offsets, it's a "self-regulation"
      val args = rest.map(_.split(":")(1))

      if (args.exists(argId =>
        id2offsets.isDefinedAt(argId) && id2offsets(argId) == id2offsets(id))) {
        for (self <- selfOption) {
          result(self.label) = eventType
          result(self.exists) = true
          result(self.argumentCandidates.head.role) = "Theme"
          result(self.argumentCandidates.head.exists) = true
        }
      }
      else {
        //find the proper relation mention on this token
        val relationMentionOption = headToken.sentence.relationMentionCandidates.find(m =>
          m.head == headToken && selfOption.getOrElse(null) != m)
        for (relationMention <- relationMentionOption) {
          var themeMatched = false
          val argsInEvent = new HashSet[Any]
          for (arg <- rest; if (arg.startsWith("Theme") || arg.startsWith("Cause") || doTask2)) {
            val Array(role, argId) = arg.split(":")
            val (beginArg, endArg) = id2offsets(argId)
            val argHead = headFinder(doc.tokenAt(beginArg).get, doc.tokenAt(endArg - 1).get)
            //determine whether to use the self mention, or the proper mention
            val argHeads = new HashSet[Token]
            argHeads += argHead
            if (copyEqui) {
              for (others <- coref.get(beginArg -> endArg); (b, e) <- others) {
                val head = headFinder(doc.tokenAt(b).get, doc.tokenAt(e - 1).get)
                argHeads += head
              }
            }
            val found = relationMention.argumentCandidates.filter(arg => argHeads(arg.arg.head))
            val activeSelf = found.find(arg => isSelfMention(arg.arg) && arg.relationMention.exists.trueValue)
            val argMentions = activeSelf.map(Seq(_)).getOrElse(found)
            val antecedents = new HashSet[EntityMention]
            val anaphora = new HashSet[EntityMention]
            for (argMention <- argMentions) {
              //            if (!found.isEmpty) {
              //              val argMention = activeSelf.getOrElse(found.head)
              val normRole = normalizeRole(role)
              //todo: check whether the entity tag is compatible with the role
              if (!doTask2 || EventSpecs.compatibleRoles(relationMention.label.trueValue, argMention)(normRole)) {
                result(argMention.role) = normRole
                result(argMention.exists) = true
                result(argMention.relationIDs) = Set(id) ++ argMention.relationIDs
                argsInEvent += argMention.arg
                themeMatched = normRole == "Theme" || themeMatched
                logger.trace("Matched argument %s".format(argId))
                if (argMention.argIsEntityMention && argMention.arg.head == argHead) {
                  antecedents += argMention.entityMention
                } else if (argMention.argIsEntityMention) {
                  anaphora += argMention.entityMention
                }
              }
            }
            //if there are any copied equi args, we make sure that they have the same entity ID.
            if (Conf.get("copyEquiIDs", true))
              for (ante <- antecedents; ana <- anaphora) {
                result(ana.entityKey) = ante.entityKey
              }
            if (argMentions.isEmpty)
              logger.trace("Couldn't match argument %s of event %s".format(arg, id))
            for (a1 <- argsInEvent; a2 <- argsInEvent; if (a1 != a2)) result(a1 -> a2) = true

          }
          if (themeMatched && eventType != "Process") {
            //todo: do we really avoid these when learning?
            result(relationMention.label) = eventType
            result(relationMention.exists) = true
            logger.trace("Matched rel mention %s".format(id))
          } else {
            //todo: what should we return here?
            result(relationMention.label) = eventType
          }

        }
      }
    }


    logger.debug("Added gold information from a2 file " + a2file.getAbsolutePath)
    result

  }

}
