package CodeGen

import CodeGen.Condition._
import CodeGen.Constants._
import CodeGen.GenerateExpressionsOptimised.generateExpressionOptimised
import CodeGen.UtilityFunctions.{sizeOfDataType}
import LexerSemanticCheck.{ASTNode, ArgListNode, ArrayElemNode, BeginNode, CallNode, ExitNode, ExprNode, FreeNode, FstNode, IdentNode, IfNode, LHSEqualsRHSNode, NewIdentAssignNode, NewPairNode, PairLiterNode, PrintNode, PrintlnNode, ReadNode, ReturnNode, SkipNode, SndNode, StatJoinNode, StatNode, StrLiterNode, TypeNode, WhileNode}

import scala.collection.mutable.ListBuffer

import scala.collection.mutable

object GenerateStatementsOptimised {

  def generateDeclaration(t: TypeNode, i: IdentNode, rhs: ASTNode, breakTo: String): mutable.ListBuffer[Instruction] = {
    StackSimulation.declareVariableOnStack(i.varName)
    ListBuffer() ++ generateAssignmentRHS(rhs, breakTo) ++
      storeResult(i, StackSimulation.offset(i.varName), t.storedType)
  }

  def generateAssignmentRHS(rhs: ASTNode, breakTo: String): mutable.ListBuffer[Instruction] = {
    rhs match {
      case CallNode(i, a) => generateCall(i, a, breakTo)
      case FstNode(e) =>
        UtilityFunctions.nullPointerFlag = true
        generateExpressionOptimised(e) ++ ListBuffer(
          BranchLink("p_check_null_pointer"),
          Load(r0, ShiftRegister(Offset(r0))),
          Load(r0, ShiftRegister(Offset(r0)))
        )
      case SndNode(e) =>
        UtilityFunctions.nullPointerFlag = true
        generateExpressionOptimised(e) ++
          ListBuffer(
            BranchLink("p_check_null_pointer"),
            Load(r0, ShiftRegister(Offset(r0, shiftVal = ImmVal(sizeOfDataType(e.storedType, isPair=true, pos=1))))),
            Load(r0, ShiftRegister(Offset(r0)))
          )
      case NewPairNode(e1, e2) =>
        generateNewPairNode(e1, e2)
      case node: ExprNode => generateExpressionOptimised(node)
      case _ => throw new UnsupportedOperationException // Unsupported cases
    }
  }

  def generateNewPairNode(e1: ExprNode, e2: ExprNode): mutable.ListBuffer[Instruction] = {
    val e1Size = sizeOfDataType(e1.storedType)
    val e2Size = sizeOfDataType(e2.storedType)

    generateExpressionOptimised(e1) ++
      generateNewPairElem(e1Size) ++
      generateExpressionOptimised(e2) ++
      generateNewPairElem(e2Size) ++
      ListBuffer(
        Load(r0, LoadImmVal(PAIR_SIZE)),
        BranchLink("malloc"),
        Pop(r2),
        Pop(r1),
        Store(r1, ShiftRegister(Offset(r0))),
        Store(r2, ShiftRegister(Offset(r0, shiftVal = ImmVal(WORD_SIZE)))),
      )
  }

  /* Element at the top of the stack points towards the value storing a pair element */
  def generateNewPairElem(size: Int): mutable.ListBuffer[Instruction] = {
    ListBuffer(
      Push(r0),
      Load(r0, LoadImmVal(size)),
      BranchLink("malloc"),
      Pop(r1),
      size match {
        case BYTE_SIZE => StoreByte(r1, ShiftRegister(Offset(r0)))
        case WORD_SIZE => Store(r1, ShiftRegister(Offset(r0)))
      },
      Push(r0)
    )
  }

  def generateAssignment(lhs: ASTNode, rhs: ASTNode, breakTo: String): mutable.ListBuffer[Instruction] = {
    lhs match {
      case node: IdentNode =>
        val offset = StackSimulation.offset(node.varName)
        val reg = node.register()

        generateAssignmentRHS(rhs, breakTo).addOne(
          node.storedType match {
            case "bool" | "char" => StoreByte(reg, ShiftRegister(Offset(fp, shiftVal = ImmVal(offset))))
            case _ => StoreByte(reg, ShiftRegister(Offset(fp, shiftVal = ImmVal(offset))))
          })

      case FstNode(expr) =>
        UtilityFunctions.nullPointerFlag = true
        generateAssignmentRHS(rhs, breakTo) ++
          generateAssignmentPair(expr)

      case SndNode(expr) =>
        UtilityFunctions.nullPointerFlag = true
        generateAssignmentRHS(rhs, breakTo) ++
          generateAssignmentPair(expr, offset=WORD_SIZE)

      case node: ArrayElemNode =>
        generateExpressionOptimised(node) ++
          ListBuffer(Push(r0)) ++
          generateAssignmentRHS(rhs, breakTo) ++
          ListBuffer(Move(r0, r5), Pop(r4),
            node.storedType match {
              case "bool" | "char" => StoreByte(r5, ShiftRegister(Offset(r4)))
              case _ => Store(r4, ShiftRegister(Offset(r4)))
            })

      case _ => throw new UnsupportedOperationException
    }
  }

  def generateAssignmentPair(e: ExprNode, offset: Int = 0): mutable.ListBuffer[Instruction] = {
    UtilityFunctions.nullPointerFlag = true
    ListBuffer(
      Push(r0)) ++
      generateExpressionOptimised(e) ++
      ListBuffer(
        BranchLink("p_check_null_pointer"),
        Load(r0, ShiftRegister(Offset(r0, shiftVal = ImmVal(offset)))),
        Pop(r1),
        e.storedType match {
          case "bool" | "char" => StoreByte(r1, ShiftRegister(Offset(r0)))
          case _ =>Store(r1, ShiftRegister(Offset(r0)))
        }
      )
  }

  def generateRead(lhs: ASTNode): mutable.ListBuffer[Instruction] = {
    val offset = lhs match {
      case ident: IdentNode => StackSimulation.offset(ident.varName)
      case _ => 0
    }

    lhs.storedType match {
      case "char" =>
        UtilityFunctions.readCharFlag = true
        UtilityFunctions.addCharMsg()
        ListBuffer(
          Add(lhs.register(), fp, ImmVal(offset)),
          BranchLink("p_read_char")
        )
      case "int" =>
        UtilityFunctions.readIntFlag = true
        UtilityFunctions.addIntMsg()
        ListBuffer(
          Add(lhs.register(), fp, ImmVal(offset)),
          BranchLink("p_read_int")
        )
    }
  }

  def generateFree(e: ExprNode, breakTo: String): mutable.ListBuffer[Instruction] = {
    UtilityFunctions.freePairFlag = true
    UtilityFunctions.nullPointerFlag = true
    generateExpressionOptimised(e) :+ BranchLink("p_free_pair")
  }

  def generateExit(e: ExprNode, breakTo: String): mutable.ListBuffer[Instruction] =
    generateExpressionOptimised(e) :+ BranchLink("exit")

  def generatePrint(e: ExprNode, printLineFlag: Boolean): mutable.ListBuffer[Instruction] = {
    val printBranch = e.storedType match {
      case "string" | "array(char)" =>
        UtilityFunctions.printStringFlag = true
        BranchLink("p_print_string")
      case "bool" =>
        if (!UtilityFunctions.printBoolFlag) {
          UtilityFunctions.addPrintTrueMsg()
          UtilityFunctions.addPrintFalseMsg()
          UtilityFunctions.printBoolFlag = true
        }
        BranchLink("p_print_bool")
      case "char" =>
        UtilityFunctions.printCharFlag = true
        BranchLink("putchar")
      case "int" =>
        if (!UtilityFunctions.printIntFlag) {
          UtilityFunctions.addIntMsg()
          UtilityFunctions.printIntFlag = true
        }
        BranchLink("p_print_int")
      case _ =>
        if (!UtilityFunctions.printRefFlag) {
          UtilityFunctions.addReferenceMsg()
          UtilityFunctions.printRefFlag = true
        }
        BranchLink("p_print_reference")
    }

    val result: ListBuffer[Instruction] = generateExpressionOptimised(e) :+ printBranch
    if (printLineFlag) {
      UtilityFunctions.printLineFlag = true
      return result :+ BranchLink("p_print_ln")
    }
    result
  }

  def generateIf(node: IfNode): mutable.ListBuffer[Instruction] = {

    val (elseBranch, endIf) = Labels.generateIfLabels()
    val cond = generateExpressionOptimised(node.e)

    val thenClause: ListBuffer[Instruction] = {
      node.symbolTableTrue match {
        case Some(table) =>
          if (table.size() > 0) {
            StackSimulation.addStackFrame(node.symbolTableTrue.get) ++
              generateStatementOptimised(node.s1, endIf) ++
              StackSimulation.freeStackFrame()
          } else if (table.size() == 0) {
            generateStatementOptimised(node.s1, endIf)
          } else {
            throw new Exception("Size of symbol table cannot be smaller than 0!")
          }
        case None => throw new Exception("Symbol table should always exist!")
      }
    }

    val elseClause: ListBuffer[Instruction] = {
      node.symbolTableFalse match {
        case Some(table) =>
          if (table.size() > 0) {
            StackSimulation.addStackFrame(node.symbolTableFalse.get) ++
              generateStatementOptimised(node.s2, endIf) ++
              StackSimulation.freeStackFrame()
          } else if (table.size() == 0) {
            generateStatementOptimised(node.s2, endIf)
          } else {
            throw new Exception("Size of symbol table cannot be smaller than 0!")
          }
        case None => throw new Exception("Else clause of if statement has no symbol table!")
      }
    }

    cond ++
      ListBuffer(
        Cmp(node.e.register(), ImmVal(0)),
        Branch(elseBranch, EQ)) ++
      thenClause ++
      ListBuffer(
        Branch(endIf),
        Label(elseBranch)) ++
      elseClause :+
      Label(endIf)
  }

  def generateWhile(node: WhileNode): mutable.ListBuffer[Instruction] = {

    val (whileStart, whileClose) = Labels.generateWhileLabel()

    val setupStack: ListBuffer[Instruction] = StackSimulation.addStackFrame(node.symbolTable.get)
    val cond: ListBuffer[Instruction] = generateExpressionOptimised(node.e)
    val body: ListBuffer[Instruction] = generateStatementOptimised(node.s, whileClose)
    val removeStack: ListBuffer[Instruction] = StackSimulation.freeStackFrame()

    setupStack ++
      ListBuffer(Label(whileStart)) ++
      cond ++
      ListBuffer(Cmp(node.e.register(), ImmVal(0)), Branch(whileClose, EQ)) ++
      body ++
      ListBuffer(Branch(whileStart), Label(whileClose)) ++
      removeStack
  }

  def generateJoin(statList: List[StatNode], breakTo: String): mutable.ListBuffer[Instruction] = {
    val result = mutable.ListBuffer[Instruction]().empty
    for (s <- statList) {
      val stat = generateStatementOptimised(s, breakTo)
      result ++= stat
    }

    result
  }

  def generateBegin(node: BeginNode, breakTo: String): mutable.ListBuffer[Instruction] = {
    val setupStack = StackSimulation.addStackFrame(node.symbolTable.get)
    val body = generateStatementOptimised(node.s, breakTo)
    val removeStack = StackSimulation.freeStackFrame()
    setupStack ++ body ++ removeStack
  }


  def generateCall(i: IdentNode, argList: ArgListNode, breakTo: String): mutable.ListBuffer[CodeGen.Instruction] = {
    val result = new mutable.ListBuffer[Instruction]().empty

    val subStackPointer = Sub(sp, sp, ImmVal(argumentsSize(argList.es)))

    var currentOffset = 0
    val pushArgs = ListBuffer[Instruction]().empty
    for (arg <- argList.es.reverse) {
      pushArgs ++= generateExpressionOptimised(arg)
      arg.storedType match {
        case "bool" | "char" =>
          pushArgs.addOne(StoreByte(r0, ShiftRegister(Offset(sp, shiftVal=ImmVal(currentOffset)))))
          currentOffset += BYTE_SIZE
        case _ =>
          pushArgs.addOne(Store(r0, ShiftRegister(Offset(sp, shiftVal=ImmVal(currentOffset)))))
          currentOffset += WORD_SIZE
      }
    }

    val funcName = i.varName
    if (!FunctionMap.functionDefined(i.varName)) {

      // mark the function as generated to avoid recursion issues
      FunctionMap.markAsGenerated(funcName)

      // get back a funcNode from just the function name
      val funcNodeRef = FunctionMap.funcRef(funcName)

      // pushing args and setting up pointers
      val setupStack = StackSimulation.addStackFrame(funcNodeRef.symbolTable.get, Some(funcNodeRef.p))

      // translate the function body
      val funcBody = generateStatementOptimised(funcNodeRef.stat, breakTo)

      // remove args and pointers
      val removeStackFrame = StackSimulation.freeStackFrame()

      // add this definition to the function map
      FunctionMap.addFunctionDef(funcName, setupStack ++ funcBody ++ removeStackFrame)
    }

    result ++= ListBuffer(subStackPointer)
    result ++= pushArgs
    result ++ ListBuffer(BranchLink(funcName), Add(sp, sp, ImmVal(argumentsSize(argList.es))))
  }

  def argumentsSize(as : List[ExprNode]) : Int = (for (a <- as) yield sizeOfDataType(a.storedType)).sum

  def generateStatementOptimised(s: StatNode, breakTo: String): mutable.ListBuffer[Instruction] = {
    s match {
      case SkipNode() => mutable.ListBuffer().empty
      case NewIdentAssignNode(t, i, rhs) => generateDeclaration(t, i, rhs, breakTo)
      case LHSEqualsRHSNode(lhs, rhs) => generateAssignment(lhs, rhs, breakTo)
      case ReadNode(lhs) => generateRead(lhs)
      case FreeNode(e) => generateFree(e, breakTo)
      case ReturnNode(e) => generateExpressionOptimised(e)
      case ExitNode(e) => generateExit(e, breakTo)
      case PrintNode(e) => generatePrint(e, printLineFlag = false)
      case PrintlnNode(e) => generatePrint(e, printLineFlag = true)
      case node: IfNode => generateIf(node)
      case node: WhileNode => generateWhile(node)
      case StatJoinNode(statList) => generateJoin(statList, breakTo)
      case node: BeginNode => generateBegin(node, breakTo)
      case CallNode(i, a) => generateCall(i, a, breakTo)
    }
  }

  def storeResult(node: IdentNode, offset: Int, dataType: String): ListBuffer[Instruction] = {
    val reg = node.register()
    ListBuffer(dataType match {
      case "bool" | "char" => StoreByte(reg, ShiftRegister(Offset(fp, shiftVal = ImmVal(offset))))
      case _ => Store(reg, ShiftRegister(Offset(fp, shiftVal = ImmVal(offset))))
    })
  }
}
