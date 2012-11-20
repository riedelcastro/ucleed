package cc.refectorie.proj.bionlp2011.semisupee

import collection.mutable.HashMap
import xml.XML
import uk.ac.ebi.kraken.uuw.services.remoting.UniProtJAPI
import cc.refectorie.proj.factorieie.annotator.{CoreNLPSentenceSplitter, CoreNLPTokenizer}
import cc.refectorie.proj.factorieie.data.{Sentence, KnowledgeBase}

/**
 * @author sriedel
 */
case class Pathway(interactions: Seq[Interaction]) {

}

trait Molecule {
  def id: String
  def names: Seq[MoleculeName]
  lazy val name = names.find(_.nameType == "PF").map(_.value)

  def proteinYield:Set[Protein]
}
case class ComponentLabel(labelType: String, value: String)
case class MoleculeName(nameType: String, longNameType: String, value: String)
case class Reference(pmid: String)
case class Protein(id: String, names: Seq[MoleculeName]) extends Molecule {
  lazy val uniprotID = names.find(_.nameType == "UP").map(_.value)
  lazy val uniprotEntry = uniprotID.map(id => UniProtResolver.getUniProtEntry(id))
  def proteinYield = Set(this)
}
case class Complex(id: String, names: Seq[MoleculeName], components: Seq[Molecule]) extends Molecule {
  def proteinYield = components.flatMap(_.proteinYield).toSet
}
case class InteractionComponent(roleType: String, molecule: Molecule, labels: Seq[ComponentLabel]) {
  override def toString =
    "%-15s %-30s %s".format(roleType, molecule.name.getOrElse("N/A"), labels.map(_.value).mkString(","))
}
case class Interaction(id:String, interactionType: String, components: Seq[InteractionComponent], references: Seq[Reference]) {
  override def toString =
    "%s\n%s\n%s".format(id,interactionType, components.mkString("\n"))
}

case class PubMedDoc(pmid: String, title: String, abstr: String)


object PathwayReader {

  def readPathway(name: String) = {
    val xml = XML.loadFile(name)
    val interactions = xml \\ "Interaction"
    val id2moleculeXML = (xml \\ "Molecule").map(x => x.attribute("id").get.text -> x).toMap
    val id2molecule = new HashMap[String, Molecule]
    def getMolecule(moleculeId: String): Molecule = {
      id2molecule.getOrElseUpdate(moleculeId, {
        val moleculeXML = id2moleculeXML(moleculeId)
        val names = for (name <- moleculeXML \\ "Name") yield
          new MoleculeName(
            name.attribute("name_type").get.text,
            name.attribute("long_name_type").get.text,
            name.attribute("value").get.text)
        val molecule = moleculeXML.attribute("molecule_type").get.text match {
          case "protein" =>
            Protein(moleculeId, names)
          case "complex" =>
            val componentsXML = moleculeXML \\ "ComplexComponent"
            val components = componentsXML.map(c => getMolecule(c.attribute("molecule_idref").get.text))
            Complex(moleculeId, names, components)
          case _ => new Molecule {
            def id = moleculeId;
            def names = Seq.empty
            def proteinYield = Set.empty
          }
        }
        molecule

      })
    }
    val result = for (interaction <- interactions) yield {
      val id = interaction.attribute("id").get.text
      val components = for (c <- (interaction \\ "InteractionComponent")) yield {
        val molecule = getMolecule(c.attribute("molecule_idref").get.text)
        val labels = (c \\ "Label").map(l =>
          ComponentLabel(l.attribute("label_type").get.text, l.attribute("value").get.text))
        InteractionComponent(c.attribute("role_type").get.text, molecule, labels)
      }
      val references = (interaction \\ "Reference").map(r =>
        Reference(r.attribute("pmid").get.text))
      Interaction(id,interaction.attribute("interaction_type").get.text, components, references)
    }
    Pathway(result)
  }


  def moleculeMentioned(molecule: Molecule, sentence: Sentence): Boolean = {
    molecule match {
      case prot@Protein(id, _) =>
        val name = prot.name.get
        sentence.text.contains(name)
      case Complex(_, _, components) =>
        components.exists(moleculeMentioned(_, sentence))
      case _ => false
    }
  }

  def atLeastNComponentsMentioned(interaction: Interaction, sentence: Sentence, n: Int = 2): Boolean = {
    var total = 0
    for (comp <- interaction.components) {
      if (moleculeMentioned(comp.molecule, sentence)) total += 1
      if (total >= n) return true
    }
    false
  }
  def atLeastNDistinctProteinsMentioned(interaction: Interaction, sentence: Sentence, n: Int = 2): Boolean = {
    var total = 0
    val proteins = interaction.components.flatMap(_.molecule.proteinYield).toSet
    for (prot <- proteins) {
      if (moleculeMentioned(prot, sentence)) total += 1
      if (total >= n) return true
    }
    false
  }
  def atLeastNDistinctProteinsFromDistinctComponentsMentioned(interaction: Interaction, sentence: Sentence, n: Int = 2): Boolean = {
    var all = Set.empty[Protein]
    var total = 0
    for (comp <- interaction.components){
      val proteins = comp.molecule.proteinYield.filter(moleculeMentioned(_,sentence))
      val added = proteins.filter(!all(_))
      if (added.size > 0) total += 1
      if (total >= n) return true
      all ++= added
    }
    false
  }




  def main(args: Array[String]) {
    val kb = new KnowledgeBase
    val pathway = readPathway("pathways/cd40_pathway.xml")
    for (interaction <- pathway.interactions) {
      println("************")
      println(interaction)
      for (ref <- interaction.references) {
        val pmid = ref.pmid
        val pmDoc = PubmedRetriever.getPubMedDoc(pmid)
        val doc = kb.createDocument("pmid", pmDoc.abstr)
        CoreNLPTokenizer.annotate(doc)
        CoreNLPSentenceSplitter.annotate(doc)
        for (sentence <- doc.sentences) {
          val matched = atLeastNDistinctProteinsFromDistinctComponentsMentioned(interaction, sentence, 2)
          if (matched) {
            println("Sentence: " + sentence.text)
          }
        }
      }
    }
  }
}

object PubmedRetriever {
  val parser = new HTML5Parser

  def getPubMedDoc(id: String) = {
    val source = new org.xml.sax.InputSource("http://www.ncbi.nlm.nih.gov/pubmed/%s?dopt=Abstract".format(id))
    val xml = parser.loadXML(source)
    val divs = xml \\ "div"
    val abstr = divs.find(_.attribute("class").map(_.text) == Some("abstr"))
    val title = xml \\ "h1"
    val abstrTxt = abstr.get.text.substring(8)
    val titleTxt = title.text
    val doc = new PubMedDoc(id, titleTxt, abstrTxt)
    doc
  }

  def main(args: Array[String]) {
    val doc = getPubMedDoc("14985354")
    println(doc.title)
    println(doc.abstr)
  }
}

object UniProtResolver {
  val entryRetrievalService = UniProtJAPI.factory.getEntryRetrievalService

  def getUniProtEntry(id: String) = {
    entryRetrievalService.getUniProtEntry(id)
  }

  def main(args: Array[String]) {
    val entry = getUniProtEntry("Q96AZ6")
    println(entry.getUniProtId.getValue)
    println(entry.getProteinDescription.getSubNames)
    println(entry.getProteinDescription.getRecommendedName)
    println(entry.getProteinDescription.getAlternativeNames)

  }
}

