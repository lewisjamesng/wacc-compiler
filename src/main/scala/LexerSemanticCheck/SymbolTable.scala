package LexerSemanticCheck

import Optimisation.InterferenceGraphNode

import scala.annotation.tailrec
import scala.collection.mutable

class SymbolTable(val parent: Option[SymbolTable], errorLog: ErrorLog) {
  val dict: mutable.Map[String, IDENTIFIER] = mutable.Map()
  val nodes: mutable.ListBuffer[InterferenceGraphNode] = mutable.ListBuffer.empty
  override def toString: String = dict.toString()

  def size(): Int = dict.size

  // CodeGen.Add new identifier into the symbol table
  def add(identName: String, t: IDENTIFIER): Unit = {
    dict.addOne(identName, t)
  }

  def addIGNode(node: ASTNode): Unit = {
    node.igNode match {
      case None =>
      case Some(igNode_) => nodes.addOne(igNode_)
    }
  }

  // Look up the identifier in the current symbol table
  def lookUp(identName: String): Option[IDENTIFIER] = {
    dict.get(identName)
  }

  // Look up the identifier in the enclosing symbol tables
  @tailrec
  final def lookUpAll(
      identName: String,
      sT: Option[SymbolTable]
  ): Option[IDENTIFIER] = {
    sT match {
      case None => None
      case Some(sTObj) =>
        val iType = sTObj.lookUp(identName)
        iType match {
          case Some(result) => Some(result)
          case None         => lookUpAll(identName, sTObj.parent)
        }
    }
  }

  // Check if types are compatible
  def typeCompat(t1: String, t2: String): Boolean = {
    if (
      t1.startsWith("array(") && t2
        .startsWith("array(") && t1.endsWith(")") && t2.endsWith(")")
    ) {
      val s1 = t1.substring(
        t1.indexOf("array(") + "array(".length,
        t1.lastIndexOf(")")
      )
      val s2 = t2.substring(
        t2.indexOf("array(") + "array(".length,
        t2.lastIndexOf(")")
      )
      return s2.equals("empty") || typeCompat(s1, s2)
    }
    if (
      t1.startsWith("pair(") && t2.startsWith("pair(") && t1.endsWith(")") && t2
        .endsWith(")")
    ) {
      val s1 =
        t1.substring(t1.indexOf("pair(") + "pair(".length, t1.lastIndexOf(")"))
      val s2 =
        t2.substring(t2.indexOf("pair(") + "pair(".length, t2.lastIndexOf(")"))
      return typeCompat(s1, s2)
    }

    t1.equals(t2) || (t1.startsWith("array") && t2.equals("array()")) ||
    (t1.startsWith("pair") && t2.equals("pair-inner") || t2.startsWith(
      "pair"
    ) && t1.equals("pair-inner")) ||
    (t1.startsWith("pair") && t2.equals("pair-liter") || t2.startsWith(
      "pair"
    ) && t1.equals("pair-liter"))
  }

  // Get the return or exit node, or return None if not found
  def checkReturnExitNode(statNode: StatNode): String = {
    statNode match {
      case stat: ReturnNode => stat.typeCheck(errorLog, this)
      case ExitNode(_)      => "exit"
      case node @ BeginNode(s) =>
        node.symbolTable match {
          case Some(symbolTable) => symbolTable.checkReturnExitNode(s)
          case None              => throw new Exception("Begin node has no symbol table")
        }
      case node @ IfNode(_, s1, s2) =>
        node.symbolTableTrue match {
          case Some(sTTrue) =>
            node.symbolTableFalse match {
              case Some(sTFalse) =>
                val r1 = sTTrue.checkReturnExitNode(s1)
                val r2 = sTFalse.checkReturnExitNode(s2)

                // Check if both branches have no return
                if (r1.equals("void") && r2.equals("void")) {
                  return "void"
                }
                r1
              case None =>
                throw new Exception(
                  "If node's false branch has no symbol table"
                )
            }
          case None =>
            throw new Exception("If node's true branch has no symbol table")
        }

      case node @ WhileNode(_, s) =>
        node.symbolTable match {
          case Some(symbolTable: SymbolTable) =>
            symbolTable.checkReturnExitNode(s)
          case _ => "Could not find a symbol table"
        }
      case StatJoinNode(statList) =>
        for (s <- statList) {
          checkReturnExitNode(s)
        }
        checkReturnExitNode(statList.last)
      case _: SkipNode => "skip"
      case _           => "void"
    }
  }

  /* Check semantics of function */
  def funcCheck(sTVal: SymbolTable, node: FuncNode): SymbolTable = {
    var sT: SymbolTable = sTVal

    /* Check if function name exists in the current scope and return type is valid */
    def checkFunctionNameAndReturnType(): Unit = {
      val f = sT.lookUp(node.i.varName)
      f match {
        /* If variable is not declared, add it to the current symbol table. */
        case None =>
          node.funcObj = Some(new FUNCTION(new TYPE(node.t.getType)))
          node.funcObj match {
            case Some(f: FUNCTION) =>
              sT.add(node.i.varName, f)
              sT.addIGNode(node)
            case _ =>
          }
        /* If variable is already declared, raise error */
        case _ =>
          errorLog.add(node.i.varName + " is already declared.")
      }
    }

    checkFunctionNameAndReturnType()

    /* CodeGen.Add function parameters to the function symbol table */
    sT = new SymbolTable(Some(sT), errorLog)
    val p = node.p
    if (p != null) {
      for (param <- p.ps) {
        sT.add(param.i.varName, new PARAM(param.t.getType))
      }
    }

    node.funcObj match {
      case None              =>
      case Some(f: FUNCTION) => f.setSymbolTable(sT)
    }

    if (node.funcObj.isEmpty) {
      return sT
    }
    node.funcObj.get.setSymbolTable(sT)

    if (p != null) {
      for (param <- p.ps) {
        node.funcObj.get.paramAppend(new PARAM(param.t.getType))
      }
    }
    node.symbolTable = Some(sT)
    sT
  }

  def findReturnNode(stat: StatNode): Boolean = {
    stat match {
      case IfNode(_, s1, s2) => findReturnNode(s1) || findReturnNode(s2)
      case WhileNode(_, s)   => findReturnNode(s)
      case BeginNode(s)      => findReturnNode(s)
      case StatJoinNode(list: List[StatNode]) =>
        list.foldLeft(false)((a, b) => a || findReturnNode(b))
      case _: ReturnNode => true
      case _             => false
    }
  }

  // Check semantics of AST nodes
  def semanticCheck(astNode: ASTNode): Unit = {
    var sT = this
    astNode match {
      case ProgramNode(funcList, programStat: StatNode) =>
        val sTList = new mutable.ListBuffer[SymbolTable].empty
        val statList = new mutable.ListBuffer[StatNode].empty
        val tList = new mutable.ListBuffer[TypeNode].empty
        val identList = new mutable.ListBuffer[IdentNode].empty

        /* Check semantics of function definitions and add them to symbol table before
         *  running semantic checks on function statements */
        for (node @ FuncNode(t, ident, _, stat) <- funcList) {
          sTList += funcCheck(sT, node)
          statList += stat
          tList += t
          identList += ident
        }

        /* Check semantics of function statements */
        for (i <- statList.indices) {
          sTList(i).semanticCheck(statList(i))

          /* Make sure that each function has the correct return type node */
          val returnType = sTList(i).checkReturnExitNode(statList(i))
          if (
            !typeCompat(tList(i).getType, returnType) && !returnType.equals(
              "exit"
            )
          ) {
            errorLog.add(
              s"The function ${identList(i).varName}'s type (${tList(i)}) is not compatible " +
                s"with return type $returnType!"
            )
          }
        }

        /* Check semantics of the program statements */
        sT.semanticCheck(programStat)

        /* CodeGen.Register an error if return statement is outside of a function */
        if (findReturnNode(programStat)) {
          errorLog.add("Returning from global scope is not allowed in WACC")
        }
        astNode.asInstanceOf[ProgramNode].symbolTable = Some(this)

      case node: BeginNode =>
        sT = new SymbolTable(Some(sT), errorLog)
        node.symbolTable = Some(sT)
        sT.semanticCheck(node.s)
        sT = sT.parent match {
          case Some(p) => p
          case None =>
            throw new Exception("Begin Node symbol table has no parent")
        }

      case node: LHSEqualsRHSNode =>
        var lhsType = node.lhs.typeCheck(errorLog, sT)
        val rhsType = node.rhs.typeCheck(errorLog, sT)
        node.lhs match {
          case ident: IdentNode =>
            val lhsIdentifier = lookUpAll(ident.varName, Some(sT))
            lhsIdentifier match {
              case Some(_: VARIABLE) | Some(_: PARAM) =>
              case _                                  => lhsType = "error"
            }
          case arrElem: ArrayElemNode =>
            arrElem.typeCheck(errorLog, sT)
          case fstNode: FstNode =>
            fstNode.typeCheck(errorLog, sT)
          case sndNode: SndNode =>
            sndNode.typeCheck(errorLog, sT)
          case _ =>
            errorLog.add(
              s"Assignment does not contain variable on lhs: ${node.lhs
                .typeCheck(errorLog, sT)}"
            )
        }
        if (!typeCompat(lhsType, rhsType)) {
          errorLog.add("Lhs and rhs are not type compatible!")
        }
      case node: NewIdentAssignNode =>
        val v = sT.lookUp(node.i.varName)

        v match {
          case Some(t) =>
            if (!t.isInstanceOf[FUNCTION]){
              errorLog.add(
                node.i.varName + " is already defined in the current scope."
              )
            }
          case None =>
            node.rhs match {
              case _: ExprNode | _: ArrayLiterNode | _: NewPairNode |
                  _: FstNode | _: SndNode | _: CallNode =>
              case _ =>
                errorLog.add(
                  "RHS of Assignment expression has the wrong node type"
                )
            }
        }
        val lhsType = node.t.getType
        val rhsType = node.rhs.typeCheck(errorLog, sT)
        if (!typeCompat(lhsType, rhsType)) {
          errorLog.add(s"Type mismatch, $lhsType $rhsType")
        } else {
          // Otherwise, check semantics of rhs node
          sT.semanticCheck(node.rhs)
          val varObj = new VARIABLE(new TYPE(node.t.getType))
          sT.add(node.i.varName, varObj)
          sT.addIGNode(node)
        }

      case node: CallNode =>
        val f = lookUpAll(node.i.varName, Some(sT))

        f match {
          case None => errorLog.add("Unknown function " + node.i.varName + ".")
          case Some(fObj: FUNCTION) =>
            if (node.a.es.length != fObj.formals.length) {
              errorLog.add("Function call with different number of arguments.")
              return
            }
            for (i <- node.a.es.indices) {
              if (
                !typeCompat(
                  node.a.es(i).typeCheck(errorLog, sT),
                  fObj.formals(i).getType
                )
              ) {
                errorLog.add(
                  "Function type incompatible with provided parameters."
                )
                return
              }
            }
            node.fObj = Some(fObj)
          case _ => errorLog.add("Not calling a function")
        }

      case node: FreeNode =>
        node.e match {
          case ident: IdentNode =>
            val varName = ident.varName
            val v = lookUpAll(varName, Some(sT))

            v match {
              case None => errorLog.add(s"$varName is not defined in scope.")
              case Some(value) =>
                val valueType = value.getType
                if (
                  !(valueType.contains("array") || valueType.contains("pair"))
                ) {
                  errorLog.add(
                    s"$varName is $valueType, not the identifier of an array or a pair."
                  )
                }
                value match {
                  case _: VARIABLE =>
                  case _: PARAM    =>
                  case _ =>
                    errorLog.add(s"$varName is not a variable or parameter.")
                }
            }
          case _ =>
            errorLog.add(
              "Argument of LexerSemanticCheck. FreeNode is not an identifier."
            )
        }

      case node @ IfNode(e, s1, s2) =>
        // Check each branch
        val sTTrue = new SymbolTable(Some(sT), errorLog)
        val sTFalse = new SymbolTable(Some(sT), errorLog)
        sTTrue.semanticCheck(s1)
        sTFalse.semanticCheck(s2)
        node.symbolTableTrue = Some(sTTrue)
        node.symbolTableFalse = Some(sTFalse)

        // Check condition
        val condT = e.typeCheck(errorLog, sT)
        if (!condT.equals("bool")) {
          errorLog.add("Conditional must be a bool type.")
        }

      case node @ WhileNode(e, s) =>
        sT = new SymbolTable(Some(sT), errorLog)
        sT.semanticCheck(s)
        node.symbolTable = Some(sT)
        val condT = e.typeCheck(errorLog, sT)
        if (!condT.equals("bool")) {
          errorLog.add("Conditional must be a bool type.")
        }

      case StatJoinNode(statList) =>
        for (s <- statList) {
          sT.semanticCheck(s)
        }
      case node: StatNode =>
        node.typeCheck(errorLog, sT)

      case node: ExprNode =>
        node.typeCheck(errorLog, sT)

      case default => default.typeCheck(errorLog, sT)
    }
  }
}
