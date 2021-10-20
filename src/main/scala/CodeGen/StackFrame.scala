package CodeGen

import CodeGen.Constants.WORD_SIZE
import CodeGen.UtilityFunctions.sizeOfDataType
import LexerSemanticCheck.{ArgListNode, FUNCTION, PARAM, ParamListNode, SymbolTable, VARIABLE}

import scala.collection.mutable

class StackFrame(symbolTable: SymbolTable, opParamList: Option[ParamListNode] = None) {
  var isFunction: Boolean = false

  var declaredVars: Set[String] = Set[String]().empty
  val varMap: mutable.Map[String, Int] = new mutable.HashMap[String, Int]().empty
  var pushedArgsSize = 0
  var localVarSize = 0

  for ((name, identObj) <- symbolTable.dict) {
    identObj match {
      case _: PARAM =>
        declaredVars += name
        pushedArgsSize += sizeOfDataType(identObj.getType)
      case _: VARIABLE =>
        varMap.addOne(name, localVarSize)
        localVarSize += sizeOfDataType(identObj.getType)
      case _ =>
    }
  }

  // Uses the functions parameter list to go backwards from the previous frame pointer
  // And add the arguments to the variable map
  opParamList match {
    case Some(paramList) =>
      isFunction = true
      // have to go past the link register and frame pointer to the arguments
      // both are of length WORD_SIZE therefore + 2 * WORD_SIZE
      var currentOffset = localVarSize + 2 * WORD_SIZE
      for (p <- paramList.ps.reverse) {
        varMap.addOne(p.i.varName, currentOffset)
        currentOffset += sizeOfDataType(p.t.getType)
      }
    case None =>
  }

  def findVar(name: String): Option[Int] = {
    if (!declaredVars.contains(name)) {
      return None
    }
    varMap.get(name)
  }
}