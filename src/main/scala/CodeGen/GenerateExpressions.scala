package CodeGen

import CodeGen.Condition._
import CodeGen.Constants._
import CodeGen.Shift._
import LexerSemanticCheck._

import scala.collection.mutable

object GenerateExpressions {

  def generateIdent(node: IdentNode): mutable.ListBuffer[Instruction] = {

    node.storedType match {
      case "bool" | "char" =>
        mutable.ListBuffer(
          LoadSByte(r0, ShiftRegister(Offset(fp, shiftVal=ImmVal(StackSimulation.offset(node.varName))))))
      case _ =>
        mutable.ListBuffer(
          Load(r0, ShiftRegister(Offset(fp, shiftVal=ImmVal(StackSimulation.offset(node.varName))))))
    }
  }

  def generateArrayElem(i: IdentNode, eList: List[ExprNode]): mutable.ListBuffer[Instruction] = {
    UtilityFunctions.checkArrayBoundsFlag = true

    val offset = StackSimulation.offset(i.varName)
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    val arrayInstructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    val arrayIndexInstr = if(i.storedType.contains("bool") || i.storedType.contains("char")) {
      Add(r4, r4, r0)
    } else {
      Add(r4, r4, ShiftRegister(PostIndex(r0, shift = Some(LSL), shiftVal = ImmVal(2))))
    }

    for (e <- eList) {
      arrayInstructions ++= generateExpression(e)
      arrayInstructions ++= List(
        Move(r1, r4),

        // index must be in r0
        // array address must be in r1

        BranchLink("p_check_array_bounds"),
        // go past the stored length at the start
        Add(r4, r4, ImmVal(WORD_SIZE)),
        // index into array
        arrayIndexInstr,
        // Load in the element
        Load(r4, ShiftRegister(Offset(r4)))
      )
    }
    instructions ++= List(
      // load array address from variable into r0
      Load(r0, ShiftRegister(Offset(fp, shiftVal=ImmVal(offset)))),

      // store address in aux r4
      Move(r4, r0))

    instructions ++= arrayInstructions
    instructions ++= List(Move(r0, r4))
    instructions
  }

  def generateArrayLiter(e: ExprNode, eList: List[ExprNode]): mutable.ListBuffer[Instruction] = {
    val arrayInstr: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    var newExprList: List[ExprNode] = List.empty
    var elemSize = 0

    if (e != null) {
      elemSize = if(e.storedType.contains("bool") || e.storedType.contains("char")) {
        BYTE_SIZE
      } else {
        WORD_SIZE
      }
      newExprList = e +: eList
      for ((e, i) <- newExprList.zipWithIndex) {
        val storeInstr: Instruction = e.storedType match {
          case "char" | "bool" =>
            StoreByte(r0, ShiftRegister(Offset(r2, shiftVal=ImmVal(i + WORD_SIZE))))
          case _ =>
            Store(r0, ShiftRegister(Offset(r2, shiftVal=ImmVal((i * WORD_SIZE) + WORD_SIZE))))
        }
        arrayInstr ++= generateExpression(e) :+ storeInstr
      }
    }
    mutable.ListBuffer(Load(r0, LoadImmVal(WORD_SIZE + newExprList.length * elemSize)),
      BranchLink("malloc"),
      Move(r2, r0)) ++
      arrayInstr ++
      mutable.ListBuffer(Load(r1, LoadImmVal(newExprList.length)),
        Store(r1, ShiftRegister(Offset(r2))),
        Move(r0, r2))
  }

  def generateBrackets(inside: ExprNode): mutable.ListBuffer[Instruction] = generateExpression(inside)

  def generateCharLiter(x: Char): mutable.ListBuffer[Instruction] =
    mutable.ListBuffer(
      Move(r0, ImmValChar(x))
    )
  def generateBoolLiter(x: Boolean): mutable.ListBuffer[Instruction] =
    mutable.ListBuffer(
      Load(r0, LoadImmVal(if (x) 1 else 0))
    )

  def generateStrLiter(s: String): mutable.ListBuffer[Instruction] = {
    val label = Labels.addDataMsg(s)
    mutable.ListBuffer(
      Load(r0, LabelOp(label))
    )
  }

  def generateIntLiter(x: Int): mutable.ListBuffer[Instruction] =
    mutable.ListBuffer(
      Load(r0, LoadImmVal(x))
    )

  def generatePairLiter(): mutable.ListBuffer[Instruction] =
    mutable.ListBuffer(
      Load(r0, LoadImmVal(0))
    )

  def generateUnaryOperation(node: UnaryOperatorNode): mutable.ListBuffer[Instruction] = {

    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    node match {
      case NotNode(expr) =>
        instructions ++= generateExpression(expr)
        instructions ++= List(
          Cmp(r0, ImmVal(0)),
          Move(r0, ImmVal(0), NE),
          Move(r0, ImmVal(1), EQ),
        )
      case NegNode(expr) =>
        UtilityFunctions.arithmeticFlag = true
        instructions ++= generateExpression(expr)
        instructions ++= List(
          ReverseSub(r0, r0, ImmVal(0)),
          BranchLink("p_throw_overflow_error", VS)
        )
      case ChrNode(expr) =>
        instructions ++= generateExpression(expr)
      case OrdNode(expr) =>
        instructions ++= generateExpression(expr)
      case LenNode(expr) =>
        instructions ++= generateExpression(expr)
        instructions ++= List(
          Load(r0, ShiftRegister(Offset(r0)))
        )
    }
    instructions
  }

  def generateBinaryOperation(node: BinaryOperatorNode): mutable.ListBuffer[Instruction] = {
    node match {
      case op: IntBinaryOperator =>
        generateArithmeticOperation(op)
      case op: BoolBinaryOperator =>
        generateBoolOperation(op)
      case op: IntCharBinaryOperator =>
        generateCompareOperation(op)
      case op: TypelessBinaryOperator =>
        generateEqualOperation(op)
    }
  }

  def generateArithmeticOperation(op: IntBinaryOperator): mutable.ListBuffer[Instruction] = {
    UtilityFunctions.arithmeticFlag = true
    /* Condition to throw an overflow error. Set to NE for SMull instruction. */
    var condForOverFlow: Condition = VS

    val binOpInstr: mutable.ListBuffer[Instruction] = op match {
      case _ : AddNode =>
        mutable.ListBuffer(
          Add(r4, r4, r5, signed=true)
        )
      case _ : SubNode =>
        mutable.ListBuffer(
          Sub(r4, r4, r5, signed=true)
        )
      case _ : MulNode =>
        condForOverFlow = NE
        mutable.ListBuffer(
          SMull(r4, r5, r4, r5),
          Cmp(r5, ShiftRegister(PostIndex(r4, shift=Some(ASR), shiftVal=ImmVal(31))))
        )
      case _ : DivNode =>
        UtilityFunctions.divisionFlag = true
        mutable.ListBuffer(
          Move(r0, r4),
          Move(r1, r5),
          BranchLink("p_check_divide_by_zero"),
          BranchLink("__aeabi_idiv"),
          Move(r4, r0)
        )
      case _ : ModNode =>
        UtilityFunctions.divisionFlag = true
        mutable.ListBuffer(
          Move(r0, r4),
          Move(r1, r5),
          BranchLink("p_check_divide_by_zero"),
          BranchLink("__aeabi_idivmod"),
          Move(r4, r1)
        )
    }

    generateExpression(op.left) ++
      mutable.ListBuffer(Push(r0)) ++
      generateExpression(op.right) ++
      mutable.ListBuffer(Move(r5, r0), Pop(r4)) ++ binOpInstr ++
      mutable.ListBuffer(BranchLink("p_throw_overflow_error", condForOverFlow), Move(r0, r4))
  }

  def generateBoolOperation(op: BoolBinaryOperator): mutable.ListBuffer[Instruction] = {

    val boolInstr: List[Instruction] = op match {
      case LANDNode(_, _) =>
        List(And(r0, r4, r5))
      case LORNode(_, _) =>
        List(Orr(r0, r4, r5))
    }

    generateExpression(op.left) ++
      List(Push(r0)) ++
      generateExpression(op.right) ++
      List(Move(r5, r0), Pop(r4)) ++
      boolInstr
  }

  def generateCompareOperation(op: IntCharBinaryOperator): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = op match {
      case GTNode(_, _) =>
        compOpBuilder.setCond(GT, LE).build(op)
      case GTENode(_, _) =>
        compOpBuilder.setCond(GE, LT).build(op)
      case LTNode(_, _) =>
        compOpBuilder.setCond(LT, GE).build(op)
      case LTENode(_, _) =>
        compOpBuilder.setCond(LE, GT).build(op)
    }

    instructions
  }

  def generateEqualOperation(op: TypelessBinaryOperator): mutable.ListBuffer[Instruction] = {

    val instructions: mutable.ListBuffer[Instruction] = op match {
      case EQNode(left, right) =>
        compOpBuilder.setCond(EQ, NE).build(op)
      case NEQNode(left, right) =>
        compOpBuilder.setCond(NE, EQ).build(op)
    }

    instructions
  }

  def generateExpression(exprNode: ExprNode): mutable.ListBuffer[Instruction] = {
    exprNode match {
      case node: IdentNode => generateIdent(node)
      case ArrayElemNode(i, eList) => generateArrayElem(i, eList)
      case ArrayLiterNode(e, eList) => generateArrayLiter(e, eList)
      case BracketsNode(inside) => generateBrackets(inside)
      case CharLiterNode(x) => generateCharLiter(x)
      case BoolLiterNode(x) => generateBoolLiter(x)
      case StrLiterNode(x) => generateStrLiter(x)
      case IntLiterNode(x) => generateIntLiter(x)
      case PairLiterNode() => generatePairLiter()
      case node: UnaryOperatorNode => generateUnaryOperation(node)
      case node: BinaryOperatorNode => generateBinaryOperation(node)
    }
  }
}
