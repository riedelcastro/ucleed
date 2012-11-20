package cc.refectorie.proj.bionlp2011

import scala.collection.JavaConversions._
import collection.mutable.{HashSet, ArrayBuffer, HashMap}
import cc.refectorie.proj.factorieie.data.RelationMentionArgument

/**
 * @author sriedel
 */

case class EventGroup(names: Seq[String], types: Set[String], task2roles: Set[String] = Set.empty,
                      task1roles: Set[String])

class EventSpecs {
  def Process = "Process"
  def Participant = "Participant"
  val ParticipantTypes = Set("Protein", "Two-component-system", "Chemical", "Organism", "Regulon-operon")

  val regulations = new HashSet[String]

  val regulationGroups = new ArrayBuffer[EventGroup]
  val nonRegulationGroups = new ArrayBuffer[EventGroup]
  val processGroups = new ArrayBuffer[EventGroup]

  val events = new ArrayBuffer[String]

  val name2group = new HashMap[String, EventGroup]

  def this(string: String) {
    this ()
    for (spec <- string.split(";")) {
      val Array(names, typesAndTask2Roles) = spec.trim.split(":")
      val (types, task2Roles) = if (typesAndTask2Roles.contains("|")) {
        val split = typesAndTask2Roles.split("\\|")
        split(0).trim -> split(1).trim
      } else typesAndTask2Roles -> ""
      val roles = if (task2Roles == "") Set.empty[String] else task2Roles.split(",").map(_.trim).toSet
      if (names.startsWith("*")) {
        addRegulationGroup(names.drop(1).split(",").map(_.trim), types.split(",").map(_.trim).toSet, roles)
      } else if (names.startsWith("$")) {
        val splitNames = names.drop(1).split(",").map(_.trim)
        val group = EventGroup(splitNames, types.split(",").map(_.trim).toSet,
          Set(BioNLPConstants.NoSite) ++ roles, Set(Participant))
        processGroups += group
        events ++= splitNames
        for (name <- splitNames) name2group(name) = group
      } else {
        addNonRegulationGroup(names.split(",").map(_.trim), types.split(",").map(_.trim).toSet, roles)

      }
    }
  }

  def addRegulationGroup(names: Seq[String], types: Set[String], task2roles: Set[String]) {
    val group = EventGroup(names, types, Set(BioNLPConstants.NoSite) ++ task2roles, Set("Theme", "Cause"))
    regulationGroups += group
    regulations ++= names
    events ++= names
    for (name <- names) name2group(name) = group
  }

  def addNonRegulationGroup(names: Seq[String], types: Set[String], task2roles: Set[String]) {
    val group = EventGroup(names, types, Set(BioNLPConstants.NoSite) ++ task2roles, Set("Theme"))
    nonRegulationGroups += group
    events ++= names
    for (name <- names) name2group(name) = group
  }

  def compatibleRoles(eventType: String, arg: RelationMentionArgument): Set[String] = {
    val group = name2group(eventType)
    if (arg.argIsEntityMention && arg.entityMention.tags(BioNLPConstants.EntityTag))
      group.task2roles
    else group.task1roles
  }

}

object EventSpecs extends EventSpecs(Conf.get[String]("events")) {

}

object TestEventSpecs {
  def main(args: Array[String]) {
    println(EventSpecs.processGroups)
  }
}