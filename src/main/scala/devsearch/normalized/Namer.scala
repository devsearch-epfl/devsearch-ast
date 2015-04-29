package devsearch.normalized

import scala.collection.mutable.{Map => MutableMap}

case class NamingError(msg: String) extends RuntimeException(msg)

/**
 * Namer
 *
 * Provides utilities to generate fresh names and maintains a mapping back
 * to their original definitions.
 */
class Namer(splitter: String = "") {
  private val nameCounters = MutableMap.empty[String, Int]
  private val originalNames = MutableMap.empty[String, String]

  def fresh(name: String): String = {
    var counter = nameCounters.getOrElseUpdate(name, 0)
    var freshName = name + splitter + (if (counter == 0 && splitter == "") "" else counter.toString)
    while (nameCounters.isDefinedAt(freshName)) {
      counter += 1
      freshName = name + splitter + counter
      nameCounters(name) = counter
    }
    originalNames(freshName) = name
    freshName
  }

  def maintain(name: String): Unit = nameCounters.get(name) match {
    case Some(i) if i > 0 =>
      throw NamingError("Can't maintain name that has already been freshened: " + name)
    case _ =>
      nameCounters(name) = 0
      originalNames(name) = name
  }
}