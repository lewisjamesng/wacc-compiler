package LexerSemanticCheck

import CodeGen.Constants.r0
import CodeGen.Register
import Optimisation.{GraphColouring, InterferenceGraph, InterferenceGraphNode}

import scala.util.control.Breaks.break
//-------------------
//    AST Class
//-------------------

sealed trait ASTNode {
  def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = "void"
  var storedType: String = "not-stored"
  var igNode: Option[InterferenceGraphNode] = None
  var index: Int = 0

  def defaultIG(varName: String): Unit = {
    val node = findIGNode(varName)
    node.start = index
    node.end = index
  }
  def findIGNode(varName: String): InterferenceGraphNode = InterferenceGraph.findIGNode(varName, index)

  def interferenceRepr()

  def printReserveRegs(): Unit = {
    if(GraphColouring.startReg < 1) {
      GraphColouring.startReg = 1
    }
  }

  def printStringReserveRegs(): Unit = {
    if(GraphColouring.startReg < 3) {
      GraphColouring.startReg = 3
    }
  }

  def register(): Register = {
    igNode match {
      case None => r0
      case Some(igNode_) => igNode_.reg
    }
  }
}

//-------------------
//     Program
//-------------------

case class ProgramNode(funcList: List[FuncNode], stat: StatNode) extends ASTNode {
  var symbolTable: Option[SymbolTable] = None

  override def interferenceRepr(): Unit = {
    for(func <- funcList) {
      func.interferenceRepr()
    }

    stat.interferenceRepr()
  }
}

//-------------------
//       Func
//-------------------

case class FuncNode(t: TypeNode, i: IdentNode, p: ParamListNode, stat: StatNode)
  extends ASTNode {
  var funcObj: Option[FUNCTION] = None
  var symbolTable: Option[SymbolTable] = None

  override def interferenceRepr(): Unit = {
    defaultIG(i.varName)

    stat.interferenceRepr()

    GraphColouring.colourGraph()
  }
}

//-------------------
//  Param-List
//-------------------

case class ParamListNode(ps: List[ParamNode])

//-------------------
//      Param
//-------------------

case class ParamNode(t: TypeNode, i: IdentNode) {
  var index: Int = 0
}

//-------------------
//       Stat
//-------------------

sealed trait StatNode extends ASTNode

case class SkipNode() extends StatNode {
  override def interferenceRepr(): Unit =
    defaultIG("skip")
}

case class NewIdentAssignNode(t: TypeNode, i: IdentNode, rhs: ASTNode)
  extends StatNode {
  override def interferenceRepr(): Unit = {
    igNode = Some(findIGNode(i.varName))
    i.igNode = igNode
    i.interferenceRepr()
    rhs.igNode = igNode
    rhs.interferenceRepr()

//    (i.igNode, rhs.igNode) match {
//      case (Some(igNode_), Some(rhsNode_)) => igNode_.addAll(rhsNode_)
//      case _ =>
//    }

    igNode.get.start = index
    // igNode = i.igNode
  }
}

case class LHSEqualsRHSNode(lhs: ASTNode, rhs: ASTNode) extends StatNode {
  var varObj: Option[VARIABLE] = None

  override def interferenceRepr(): Unit = {
    /* TODO: Reconsider Implementation */
  }
}

case class ReadNode(lhs: ASTNode) extends StatNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    val lhsType = lhs.typeCheck(errorLog, symbolTable)
    if(!lhsType.equals("int") && !lhsType.equals("char")){
      errorLog.add("Read only takes int or char types")
    }
    "void"
  }

  override def interferenceRepr(): Unit = {
    lhs.interferenceRepr()
    igNode = lhs.igNode
  }
}

case class FreeNode(e: ExprNode) extends StatNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    e.typeCheck(errorLog, symbolTable)
    "void"
  }

  override def interferenceRepr(): Unit = {
    e.interferenceRepr()
    igNode = e.igNode

    printStringReserveRegs()
  }
}

case class ReturnNode(e: ExprNode) extends StatNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    val eType = e.typeCheck(errorLog, symbolTable)
    storedType = eType
    storedType
  }

  override def interferenceRepr(): Unit = {
    e.interferenceRepr()
    igNode = e.igNode
  }
}

case class ExitNode(e: ExprNode) extends StatNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    val eType = e.typeCheck(errorLog, symbolTable)
    if(!eType.equals("int")) {
      errorLog.add("exit code must be int types")
    }
    storedType = eType
    storedType
  }

  override def interferenceRepr(): Unit = {
    e.interferenceRepr()
    igNode = e.igNode
  }
}

case class PrintNode(e: ExprNode) extends StatNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    if (e.typeCheck(errorLog, symbolTable).contains("array()")) {
      errorLog.add("You cannot directly print a char[] in WACC")
    }
    "void"
  }

  override def interferenceRepr(): Unit = {
    e.interferenceRepr()
    igNode = e.igNode
  }
}

case class PrintlnNode(e: ExprNode) extends StatNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String  = {
    e.typeCheck(errorLog, symbolTable)
    "void"
  }

  override def interferenceRepr(): Unit = {
    e.interferenceRepr()
    igNode = e.igNode
  }
}

case class IfNode(e: ExprNode, s1: StatNode, s2: StatNode) extends StatNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    s1.typeCheck(errorLog, symbolTable)
    s2.typeCheck(errorLog, symbolTable)
    "void"
  }
  var symbolTableTrue: Option[SymbolTable] = None
  var symbolTableFalse: Option[SymbolTable] = None

  override def interferenceRepr(): Unit = {
    e.interferenceRepr()
    igNode = e.igNode
    s1.interferenceRepr()
    s2.interferenceRepr()
  }
}

case class WhileNode(e: ExprNode, s: StatNode) extends StatNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    s.typeCheck(errorLog, symbolTable)
  }
  var symbolTable: Option[SymbolTable] = None

  override def interferenceRepr(): Unit = {
    e.interferenceRepr()
    igNode = e.igNode

    s.interferenceRepr()
    igNode match {
      case None =>
      case Some(node) =>
        s.igNode match {
          case None =>
          case Some(neighbour) => node.addEdge(neighbour)
        }
    }
  }
}

case class StatJoinNode(statList: List[StatNode]) extends StatNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    for(s <- statList) {
      s.typeCheck(errorLog, symbolTable)
    }
    "void"
  }

  override def interferenceRepr(): Unit = {
    for(stat <- statList) {
      stat.interferenceRepr()
      igNode match {
        case None => igNode = stat.igNode
        case _ =>
      }
    }
  }
}

case class BeginNode(s: StatNode) extends StatNode {

  var symbolTable: Option[SymbolTable] = None
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    s.typeCheck(errorLog, symbolTable)
    "void"
  }

  override def interferenceRepr(): Unit = {
    defaultIG("begin")
    s.interferenceRepr()
  }
}

//-------------------
//    Assign-lhs
//-------------------

case class IdentNode(varName: String) extends ExprNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    val I = symbolTable.lookUpAll(varName, Option(symbolTable))

    if (I.isEmpty) {
      errorLog.add("Identifier " + varName + " is not in scope.")
      return "error"
    }
    else if (!I.get.isInstanceOf[VARIABLE] && !I.get.isInstanceOf[FUNCTION] && !I.get.isInstanceOf[PARAM]) {
      errorLog.add(varName + " isn't a variable, function or parameter")
      return "error"
    }

    storedType = I.get match {
      case variable: VARIABLE => variable.getType
      case function: FUNCTION => function.getType
      case parameter: PARAM => parameter.getType
    }
    storedType
  }

  override def interferenceRepr(): Unit = {
    igNode = Some(findIGNode(varName))

    igNode match {
      case None =>
      case Some(igNode_) =>
        if(igNode_.end < index) {
          igNode_.end = index
        }
    }
  }
}

case class ArrayElemNode(i: IdentNode, eList: List[ExprNode]) extends ExprNode {
  //x[1][2][3]
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    val T = eList.head.typeCheck(errorLog, symbolTable)

    val v = symbolTable.lookUpAll(i.varName, Option(symbolTable))

    // Not allowed to access elements of a string
    if(v.isDefined && v.get.getType.equals("string")) {
      errorLog.add("Element access is not permitted for strings")
      return "string"
    }

    for (es <- eList) {
      if (!es.typeCheck(errorLog, symbolTable).equals(T)) {
        errorLog.add("Type mismatch in array")
        break
      }
    }
    var arrayType = i.typeCheck(errorLog, symbolTable)

    while(arrayType.startsWith("array(")) {
      arrayType = arrayType.substring(arrayType.indexOf("array(") + "array(".length, arrayType.lastIndexOf(")"))
    }
    storedType = arrayType
    storedType
  }

  override def interferenceRepr(): Unit = {
    igNode = Some(findIGNode(i.varName))

    igNode match {
      case None =>
      case Some(igNode_) =>
        if(igNode_.start == 0 || igNode_.start > index) {
          igNode_.start = index
        }
    }

    val arrayAddress = findIGNode(s"array_address${findPosition()}")

    igNode match {
      case None =>
      case Some(igNode_) =>
        arrayAddress.addEdge(igNode_)
    }

    for(e <- eList) {
      e.igNode = Some(arrayAddress)
    }

    /* Reserve registers for Out of Bound error */
    printStringReserveRegs()

    igNode match {
      case None =>
      case Some(igNode_) =>
        if(igNode_.end < index) {
          igNode_.end = index
        }
    }
  }

  def findPosition(): String = {

    var result: String = "["

    for(e <- eList) {
      val str = e match {
        case IdentNode(varName) => varName
        case IntLiterNode(x) => x
        case _ =>
      }
      result += str
    }
    result += "]"
    result
  }
}

//-------------------
//    Assign-rhs
//-------------------

sealed trait ExprNode extends ASTNode {
  def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String
}

case class ArrayLiterNode(e: ExprNode, eList: List[ExprNode]) extends ExprNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    if (e == null) {
      return "array(empty)"
    }
    val T = e.typeCheck(errorLog, symbolTable)
    for (es <- eList) {
      if (!es.typeCheck(errorLog, symbolTable).equals(T)) {
        errorLog.add("Array elements are not the same type")
        break
      }
    }
    storedType = "array(" + T + ")"
    storedType
  }

  override def interferenceRepr(): Unit = {
    /* TODO: Reconsider implementation */
  }
}

case class NewPairNode(e1: ExprNode, e2: ExprNode) extends ASTNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = "pair-liter"
  storedType = "pair-liter"

  override def interferenceRepr(): Unit = {
    /* TODO: Reconsider Implementation */
  }
}

case class CallNode(i: IdentNode, a: ArgListNode) extends StatNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    storedType = i.typeCheck(errorLog, symbolTable)
    storedType
  }

  var fObj: Option[FUNCTION] = None

  override def interferenceRepr(): Unit = {
    /* TODO: Reconsider Implementation */
    a.igNode = igNode
    a.interferenceRepr()
  }
}

//-------------------
//     Arg-list
//-------------------

case class ArgListNode(es: List[ExprNode]) extends ASTNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    for (e <- es) e.typeCheck(errorLog, symbolTable)
    "void"
  }

  override def interferenceRepr(): Unit = {
    var set: Boolean = false
    for(e <- es) {
      e match {
        case node: BinaryOperatorNode =>
          node.interferenceRepr()
          if(!set) {
            igNode = e.igNode
            set = true
          }
        case _ => e.igNode = igNode
      }
    }
  }
}

//-------------------
//    Pair-elem
//-------------------

case class FstNode(e: ExprNode) extends ASTNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    val pair = e.typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable)
    if(pair.equals("pair-liter")) {
      errorLog.add("Cannot do fst of null")
      return "error"
    }
    val result = pair.substring(pair.indexOf("(") + 1, pair.indexOf(","))
    storedType = result
    storedType
  }

  override def interferenceRepr(): Unit = {
    /* TODO: Reconsider Implementation */
  }
}

case class SndNode(e: ExprNode) extends ASTNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    val pair = e.typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable)
    if(pair.equals("pair-liter")) {
      errorLog.add("Cannot do fst of null")
      return "error"
    }
    val result = pair.substring(pair.indexOf(",") + 1, pair.lastIndexOf(")"))
    storedType = result
    storedType
  }
  override def interferenceRepr(): Unit = {
    /* TODO: Reconsider Implementation */
  }
}

//-------------------
//       Type
//-------------------

sealed trait TypeNode extends ASTNode {
  def getType: String
  override def interferenceRepr(): Unit = {}
}

//-------------------
//     Base-Type
//-------------------

sealed trait BaseTypeNode extends TypeNode {
}

case class IntBaseTypeNode() extends BaseTypeNode {
  override def getType: String = "int"
  storedType = "int"
}

case class CharBaseTypeNode() extends BaseTypeNode {
  override def getType: String = "char"
  storedType = "char"
}

case class StringBaseTypeNode() extends BaseTypeNode {
  override def getType: String = "string"
  storedType = "string"
}

case class BoolBaseTypeNode() extends BaseTypeNode {
  override def getType: String = "bool"
  storedType = "bool"
}

//-------------------
//   Array-type
//-------------------

case class ArrayTypeNode(t: TypeNode) extends TypeNode {
  override def getType: String = {
    storedType = "array(" + t.getType + ")"
    storedType
  }
}

//-------------------
//   Pair-type
//-------------------

case class PairTypeNode(t1: PairElemTypeNode, t2: PairElemTypeNode)
  extends TypeNode {
  override def getType: String = {
    storedType = "pair(" + t1.getType + "," + t2.getType + ")"
    storedType
  }
}

//-------------------
//   Pair-Elem-Type
//-------------------

sealed trait PairElemTypeNode extends TypeNode

case class PairElemTypeBaseArrayNode(t: TypeNode) extends PairElemTypeNode {
  override def getType: String = {
    storedType = t.getType
    storedType
  }
}

case class PairElemTypePairNode() extends PairElemTypeNode {
  override def getType: String = "pair-inner"
  storedType = "pair_inner"
}

//-------------------
//    Expressions
//-------------------

case class BracketsNode(inside: ExprNode) extends ExprNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    storedType = inside.typeCheck(errorLog, symbolTable)
    storedType
  }

  override def interferenceRepr(): Unit = {}
}

case class CharLiterNode(x: Char) extends ExprNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    storedType = "char"
    storedType
  }
  override def interferenceRepr(): Unit = {
    /* TODO: Reconsider Implementation */
  }
}

case class BoolLiterNode(x: Boolean) extends ExprNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    storedType = "bool"
    storedType
  }

  override def interferenceRepr(): Unit = {}
}

case class StrLiterNode(x: String) extends ExprNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    storedType = "string"
    storedType
  }
  override def interferenceRepr(): Unit = {
    /* TODO: Reconsider Implementation */
  }
}

case class IntLiterNode(x: Int) extends ExprNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    storedType = "int"
    storedType
  }

  override def interferenceRepr(): Unit = {}
}

case class PairLiterNode() extends ExprNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    storedType = "pair-liter"
    storedType
  }

  override def interferenceRepr(): Unit = {
    defaultIG("null")
  }
}

//-------------------
//  Unary Operators
//-------------------

sealed trait UnaryOperatorNode extends ExprNode {
  val inner: ExprNode
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    storedType = inner.typeCheck(errorLog, symbolTable)
    storedType
  }

  override def interferenceRepr(): Unit = {
    inner.interferenceRepr()
    igNode = inner.igNode
  }
}

case class OrdNode(inner: ExprNode) extends UnaryOperatorNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    if (!inner.typeCheck(errorLog, symbolTable).equals("char")) {
      errorLog.add("Ord has a non-char argument")
    }
    storedType = "int"
    storedType
  }
}

case class ChrNode(inner: ExprNode) extends UnaryOperatorNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    if (!inner.typeCheck(errorLog, symbolTable).equals("int")) {
      errorLog.add("Chr has a non-int argument")
    }
    storedType = "char"
    storedType
  }
}

case class NegNode(inner: ExprNode) extends UnaryOperatorNode

case class NotNode(inner: ExprNode) extends UnaryOperatorNode

case class LenNode(inner: ExprNode) extends UnaryOperatorNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    if (!inner.typeCheck(errorLog, symbolTable).startsWith("array")) {
      errorLog.add("Len has non array argument")
    }
    storedType = "int"
    storedType
  }
}

//-------------------
// Binary Operators
//-------------------

sealed trait BinaryOperatorNode extends ExprNode {
  def successType: String = "bool"
  val validTypes: List[String]

  val left: ExprNode
  val right: ExprNode

  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    val leftType = left.typeCheck(errorLog, symbolTable)
    val rightType = right.typeCheck(errorLog, symbolTable)
    if (!symbolTable.typeCompat(leftType, rightType)) {
      errorLog.add("Type mismatch in binary expression")
    }

    if(!validTypes.contains(leftType)) {
      errorLog.add("Operator does not support operations on " + leftType)
    }
    storedType = successType
    storedType
  }

  override def interferenceRepr(): Unit = {
    left.interferenceRepr()
    right.interferenceRepr()
    igNode = left.igNode
    (left.igNode, right.igNode) match {
      case (Some(left_), Some(right_)) =>
        left_.addEdge(right_)
        if(left_.end < index) {
          left_.end = index
        }
        if(right_.end < index) {
          right_.end = index
        }
      case (Some(left_), _) =>
        if(left_.end < index) {
          left_.end = index
        }
      case (_, Some(right_)) =>
        if(right_.end < index) {
          right_.end = index
        }
      case _ =>
    }
  }
}

sealed trait IntBinaryOperator
  extends BinaryOperatorNode {
  override def successType = "int"
  override val validTypes: List[String] = List("int")
}

sealed trait IntCharBinaryOperator
  extends BinaryOperatorNode {
  override val validTypes: List[String] = List("int", "char")
}

sealed trait TypelessBinaryOperator
  extends BinaryOperatorNode {
  override def typeCheck(errorLog: ErrorLog, symbolTable: SymbolTable): String = {
    if (!symbolTable.typeCompat(left.typeCheck(errorLog, symbolTable), right.typeCheck(errorLog, symbolTable))) {
      errorLog.add("Type mismatch in binary operator")
    }
    storedType = successType
    storedType
  }

  override val validTypes: List[String] = Nil
}

sealed trait BoolBinaryOperator
  extends BinaryOperatorNode {
  override val validTypes: List[String] = List("bool")
}

// precedence 1
case class MulNode(left: ExprNode, right: ExprNode)
  extends IntBinaryOperator

case class DivNode(left: ExprNode, right: ExprNode)
  extends IntBinaryOperator

case class ModNode(left: ExprNode, right: ExprNode)
  extends IntBinaryOperator

// precedence 2
case class AddNode(left: ExprNode, right: ExprNode)
  extends IntBinaryOperator

case class SubNode(left: ExprNode, right: ExprNode)
  extends IntBinaryOperator

// precedence 3
case class GTNode(left: ExprNode, right: ExprNode)
  extends IntCharBinaryOperator

case class GTENode(left: ExprNode, right: ExprNode)
  extends IntCharBinaryOperator

case class LTNode(left: ExprNode, right: ExprNode)
  extends IntCharBinaryOperator

case class LTENode(left: ExprNode, right: ExprNode)
  extends IntCharBinaryOperator

// precedence 4
case class EQNode(left: ExprNode, right: ExprNode)
  extends TypelessBinaryOperator

case class NEQNode(left: ExprNode, right: ExprNode)
  extends TypelessBinaryOperator

// precedence 5
case class LANDNode(left: ExprNode, right: ExprNode)
  extends BoolBinaryOperator

// precedence 6
case class LORNode(left: ExprNode, right: ExprNode)
  extends BoolBinaryOperator
