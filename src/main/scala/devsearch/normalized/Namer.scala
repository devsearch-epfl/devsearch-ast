package devsearch.normalized

import scala.collection.mutable.{Map => MutableMap}

/** Thrown by [[Namer]] when it enters an invalid state */
case class NamingError(msg: String) extends RuntimeException(msg)

/** Namer
  *
  * Provides utilities to generate fresh names and maintains a mapping back
  * to their original definitions.
  */
class Namer(splitter: String = "") {
  private val nameCounters = MutableMap.empty[String, Int]
  private val originalNames = MutableMap.empty[String, String]

  /** Returns a fresh name. Tries to keep it nice and readable when possible */
  def fresh(name: String): String = {
    var counter = nameCounters.get(name).map(_ + 1) getOrElse 0
    var freshName = name + (if (counter == 0) "" else splitter + counter.toString)
    while (nameCounters.isDefinedAt(freshName)) {
      counter += 1
      freshName = name + splitter + counter
      nameCounters(name) = counter
    }
    originalNames(freshName) = name
    nameCounters(name) = counter
    freshName
  }

  /** Registers a name that cannot be used as a fresh name.
    *
    * Note that this method will throw a [[NamingError]] if the name to maintain has
    * already been used as a fresh name (can't be guaranteed maintained if that's the case).
    */
  def maintain(name: String): Unit = nameCounters.get(name) match {
    case Some(i) if i > 0 =>
      throw NamingError("Can't maintain name that has already been freshened: " + name)
    case _ =>
      nameCounters(name) = 0
      originalNames(name) = name
  }
}
