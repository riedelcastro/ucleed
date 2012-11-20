package cc.refectorie.proj.bionlp2011

import cc.refectorie.proj.factorieie.annotator.Annotator
import java.io._
import io.Source
import cc.refectorie.proj.factorieie.util.languages.sexp.{SExp, SParser}
import cc.refectorie.proj.factorieie.data.{Sentence, Tree, Token, Document}
import cc.refectorie.proj.factorieie.util.HasLogger
import cc.refectorie.proj.factorieie.util.languages.sexp.SExpSyntax.{Z, L, S}
import collection.mutable.ArrayBuffer

/**
 * @author sriedel
 */

class McCloskyParser(parseName: String = "mcclosky", parserDir: File,
                     modelFile: File, maxLength: Int = 200, useK: Boolean = true) extends Annotator with HasLogger {

  val strictMatching = Conf.get("strictParseMatching", false)

  val targetParseDir = Conf.get("mccloskyTargetDir", new File("/tmp/parseOut"))

  case class TreeNode(begin: Token, end: Token, label: String, children: Seq[TreeNode]) {
    def isPreterminal = children.size == 1 && children.head.children.isEmpty

    def isTerminal = children.isEmpty

    override def toString = "Node(%s,%s,%s,%s)".format(
      label, begin.indexInDocument, end.indexInDocument, children.mkString(","))
  }

  private def buildChildren(sexps: Seq[SExp], sentence: Sentence, offset: Int): Seq[TreeNode] = {
    if (sexps.size == 0) Seq.empty
    else {
      val first = buildTreeNode(sexps.head, sentence, offset)
      Seq(first) ++ buildChildren(sexps.drop(1), sentence, first.end.indexInSentence + 1)
    }
  }

  private def buildTreeNode(sexp: SExp, sentence: Sentence, offset: Int = 0): TreeNode = {
    val result = sexp match {
      case L(Seq(S(label), rest@_*)) => {
        val children = buildChildren(rest, sentence, offset)
        TreeNode(children.head.begin, children.last.end, label, children)
      }
      case S(text) =>
        TreeNode(sentence.tokens(offset), sentence.tokens(offset), text, Seq.empty)
      case Z(number) =>
        TreeNode(sentence.tokens(offset), sentence.tokens(offset), number.toString, Seq.empty)
    }
    val matched = !result.isTerminal || result.label.startsWith("-") ||
      unnormalize(result.label) == unnormalize(result.begin.word)
    assert(matched, "Words don't match: %s != %s".format(unnormalize(result.label), result.begin.word))
    result

  }

  /**
   * Also fills up "tag" properties of tokens with POS tags.
   */
  private def fillUpParseAndTags(root: Tree, treeNode: TreeNode) {
    for (childNode <- treeNode.children) {
      val childTree = root.createChild(childNode.label, childNode.begin, childNode.end)
      if (childNode.isPreterminal) {
        childNode.begin.tag = childNode.label
        fillUpParseAndTags(childTree, childNode)
      } else {
        fillUpParseAndTags(childTree, childNode)
      }
    }
  }

  private def normalize(word: String) = {
    if (useK) {
      if (word.size > 1) word match {
        case w if (word.contains(':')) => word.replaceAll(":", "-COLON-")
        case w if (word.contains(',')) => word.replaceAll(",", "-COMMA-")
        case w if (word.contains('.')) => word.replaceAll("\\.", "&PD")
        case w if (word.contains(' ')) => word.replaceAll(":", "-WS-")
        case _ => word
      } else word
    }
    else if (word.size > 1) word match {
      case w if (word.contains(',')) => word.replaceAll(",", "-COMMA-")
      case w if (word.contains(':')) => word.replaceAll(":", "-COLON-")
      case w if (word.contains('.')) => word.replaceAll("\\.", "&PD")
      case _ => word
    } else word
  }

  private def unnormalize(word: String) = {
    if (useK) word match {
      case "''" => "\""
      case w if (w.contains("-COLON-")) => word.replaceAll("-COLON-", ":")
      case w if (w.contains("-COMMA-")) => word.replaceAll("-COMMA-", ",")
      case w if (w.contains("&PD")) => word.replaceAll("&PD", ".")
      case w if (w.contains("-WS-")) => word.replaceAll("-WS-", " ")
      case _ => word
    }
    else
      word match {
        case "''" => "\""
        case w if (w.contains("-COMMA-")) => word.replaceAll("-COMMA-", ",")
        case w if (w.contains("-COLON-")) => word.replaceAll("-COLON-", ":")
        case w if (w.contains("&PD")) => word.replaceAll("&PD", ".")
      }
  }

  def annotate(doc: Document): Unit = {
    //create temporary text file
    val loadParses = Conf.get("loadParses", false)
    def filter(sentence: Sentence): Boolean = {
      loadParses || sentence.tokens.size <= maxLength && sentence.tokens.size > 1
    }
    val filtered = doc.sentences.filter(filter(_))
    val removed = doc.sentences.filter(!filter(_))
    if (filtered.size < doc.sentences.size) {
      logger.info("Filtered %d sentences with lengths: %s".format(removed.size, removed.map(_.tokens.size).mkString(",")))
    }

    val lines = new ArrayBuffer[String]

    if (!loadParses) {

      val tmp = File.createTempFile(doc.id.replaceAll("/", "_"), "-mcclosky.txt")
      val tmpCandidates = File.createTempFile(doc.id.replaceAll("/", "_"), "-mcclosky.cand")
      val out = new PrintStream(tmp)
      val outCandidates = new PrintStream(tmpCandidates)
      for (sentence <- filtered) {
        out.print("<s> ")
        out.println(sentence.tokens.map(t => normalize(t.word)).mkString(" "))
        out.println(" </s>")
      }
      out.close
      val parser = parserDir.getAbsolutePath
      val model = modelFile.getAbsolutePath

      val firstPassCmd = "%s/first-stage/PARSE/parseIt -l399 -N50%s %s/parser/ %s".format(
        parser, if (useK) " -K" else "", model, tmp.getAbsolutePath)
      val secondPassCmd = "%s/second-stage/programs/features/best-parses -l %s/reranker/features.bz2 %s/reranker/weights.bz2".format(
        parser, model, model)

      //    println(tmp.getAbsolutePath)
      //    println(tmpCandidates.getAbsolutePath)

      //start parser
      val firstPass = Runtime.getRuntime().exec(firstPassCmd)

      //write result into candidate file
      for (line <- Source.fromInputStream(firstPass.getInputStream).getLines) {
        outCandidates.println(line)
      }

      //close input stream from parser
      firstPass.getInputStream.close

      //read in all candidate parse lines
      val candidateLines = Source.fromFile(tmpCandidates).getLines.toArray

      //read in error messages from parser
      for (line <- Source.fromInputStream(firstPass.getErrorStream).getLines)
        logger.info("First Pass Error Stream: " + line)

      //now start the reranker
      val secondPass = Runtime.getRuntime().exec(secondPassCmd)

      //get the output stream to provide parse candidates to the reranker
      val secondPassOut = new PrintStream(secondPass.getOutputStream)

      //start a thread to pass candidates to reranker
      new Thread(new Runnable {
        def run = {
          secondPassOut.print(candidateLines.mkString("\n"))
          secondPass.getOutputStream.close
        }
      }).start

      //start a thread to read rerankers error messages //todo: maybe this doesn't need a thread if done later
      new Thread(new Runnable {
        def run = {
          for (line <- Source.fromInputStream(secondPass.getErrorStream).getLines)
            logger.info("Second Pass Error Stream: " + line)
          secondPass.getErrorStream.close

        }
      }).start

      //get the selected parse trees
      lines ++= Source.fromInputStream(secondPass.getInputStream).getLines

      assert(lines.size == filtered.size)

      secondPass.getInputStream.close
      firstPass.getErrorStream.close

      secondPass.waitFor

      targetParseDir.mkdirs
      val parseOutFile = new File(targetParseDir,BioNLPUtils.fileName(doc.id) + ".ptb")
      val parseOut = new PrintStream(parseOutFile)
      for (line <- lines) {
        parseOut.println(line)
      }
      parseOut.close

    } else {
      val parseDir = Conf.get[File]("parseDir")
      val ptbFile = new File(parseDir, BioNLPUtils.fileName(doc.id) + ".ptb")
      lines ++= Source.fromFile(ptbFile).getLines
    }

    var sexp: SExp = null
    //get results and create tree
    for ((line, sent) <- lines.zip(filtered)) {
      try {
        sexp = SParser.parse(line)(0)
        val tree = buildTreeNode(sexp, sent, 0)
        val root = sent.createParseTree(parseName, tree.label)
        fillUpParseAndTags(root, tree)
      } catch {
        case e => {
          println(sent.source)
          if (strictMatching)
            throw new RuntimeException("Problem with sentence %s,\nparse %s\n and sexp parse %s".
              format(sent.text, line, sexp), e)
          else
            logger.warn("Problem with sentence %s,\nparse %s\n and sexp parse %s".
              format(sent.text, line, sexp))
        }
      }
    }

    logger.info("Parsed document %s".format(doc.id))
  }
}

/*
${RERANKINGPARSER}/first-stage/PARSE/parseIt -l399 -N50 ${BIOPARSINGMODEL}/parser/ ${FILETOPARSE} |
${RERANKINGPARSER}/second-stage/programs/features/best-parses -l ${BIOPARSINGMODEL}/reranker/features.gz ${BIOPARSINGMODEL}/reranker/weights.gz
*/