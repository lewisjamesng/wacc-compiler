package CodeGen

import CodeGen.Constants.{pc, lr}
import LexerSemanticCheck.FuncNode

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object FunctionMap {

  var funcRef: Map[String, FuncNode] = Map.empty

  private val functionMap = new mutable.HashMap[String, ListBuffer[Instruction]]()
  private val functionGenerated = new mutable.HashSet[String]().empty

  def markAsGenerated(name: String) : Unit = functionGenerated += name
  def addFunctionDef(name: String, definition: ListBuffer[Instruction]): Unit = functionMap += name -> definition
  def functionDefined(name: String): Boolean = functionGenerated.contains(name)

  def getAllUserFunctions: List[ListBuffer[Instruction]] = {
    (for ((name , body) <- functionMap) yield Label(name) +: Push(lr) +: body :+ Pop(pc) :+ Directive("ltorg")).toList
  }
}