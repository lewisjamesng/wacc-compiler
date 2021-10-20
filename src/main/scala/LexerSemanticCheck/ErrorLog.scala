package LexerSemanticCheck

import scala.collection.mutable

trait ErrorLog {
  val errorType: String
  private val log = mutable.ListBuffer[String]()

  // CodeGen.Add errors to the log
  def add(s: String): Unit = {
    log += s
  }

  // Print errors found in the log
  def printErrors(): Unit = {
    val logSize = log.length
    println(s"$logSize errors found.")
    // Format errors and print
    log.map(Console.RED ++ Console.BOLD ++ s"[$errorType Error] " ++ Console.RESET ++ _).foreach(println)
  }

  def empty(): Unit = {
    log.clear()
  }

  def thereAreErrors: Boolean = log.nonEmpty
}

// Log for semantic errors
object SemanticErrorLog extends ErrorLog {
  override val errorType = "Semantic"
}

// Log for syntax errors -- Not Used
object SyntaxErrorLog extends ErrorLog {
  override val errorType = "Syntax"
}