package CodeGen

import CodeGen.Condition.{Condition, EQ, GE, GT, LE, LT, NE, VS}
import CodeGen.Constants._
import CodeGen.Shift.{ASR, LSL}
import LexerSemanticCheck._

import scala.collection.mutable

object GenerateExpressionsOptimised {

  def generateIdent(node: IdentNode): mutable.ListBuffer[Instruction] = {

    val reg = node.register()

    node.storedType match {
      case "bool" | "char" =>
        mutable.ListBuffer(
          LoadSByte(reg, ShiftRegister(Offset(fp, shiftVal = ImmVal(StackSimulation.offset(node.varName))))))
      case _ =>
        mutable.ListBuffer(
          Load(reg, ShiftRegister(Offset(fp, shiftVal = ImmVal(StackSimulation.offset(node.varName))))))
    }
  }

  def generateArrayElem(node: ArrayElemNode): mutable.ListBuffer[Instruction] = {
    UtilityFunctions.checkArrayBoundsFlag = true

    val reg = node.register()
    val offset = StackSimulation.offset(node.i.varName)
    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    val arrayInstructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    val arrayIndexInstr = if (node.i.storedType.contains("bool") || node.i.storedType.contains("char")) {
      Add(reg, reg, r0)
    } else {
      Add(reg, reg, ShiftRegister(PostIndex(r0, shift = Some(LSL), shiftVal = ImmVal(2))))
    }

    for (e <- node.eList) {
      val indexReg =
        arrayInstructions ++= generateExpressionOptimised(e)
      arrayInstructions ++= List(
        Move(r1, reg),

        // index must be in r0
        // array address must be in r1

        BranchLink("p_check_array_bounds"),
        // go past the stored length at the start
        Add(reg, reg, ImmVal(WORD_SIZE)),
        // index into array
        arrayIndexInstr,
        // Load in the element
        Load(reg, ShiftRegister(Offset(reg)))
      )
    }
    instructions ++= List(
      // load array address from variable into r0
      Load(r0, ShiftRegister(Offset(fp, shiftVal = ImmVal(offset)))),

      // store address in aux r4
      Move(reg, r0))

    instructions ++= arrayInstructions
    instructions ++= List(Move(r0, reg))
    instructions
  }

  def generateArrayLiter(node: ArrayLiterNode): mutable.ListBuffer[Instruction] = {
    val arrayInstr: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    var newExprList: List[ExprNode] = List.empty
    var elemSize = 0
    val reg = node.register()

    if (node.e != null) {
      elemSize = if (node.e.storedType.contains("bool") || node.e.storedType.contains("char")) {
        BYTE_SIZE
      } else {
        WORD_SIZE
      }
      newExprList = node.e +: node.eList
      for ((e, i) <- newExprList.zipWithIndex) {
        val storeInstr: Instruction = e.storedType match {
          case "char" | "bool" =>
            StoreByte(r0, ShiftRegister(Offset(reg, shiftVal = ImmVal(i + WORD_SIZE))))
          case _ =>
            Store(r0, ShiftRegister(Offset(reg, shiftVal = ImmVal((i * WORD_SIZE) + WORD_SIZE))))
        }
        arrayInstr ++= generateExpressionOptimised(e) :+ storeInstr
      }
    }
    mutable.ListBuffer(Load(r0, LoadImmVal(WORD_SIZE + newExprList.length * elemSize)),
      BranchLink("malloc"),
      Move(reg, r0)) ++
      arrayInstr ++
      mutable.ListBuffer(Load(reg, LoadImmVal(newExprList.length)),
        Store(reg, ShiftRegister(Offset(reg))),
        Move(r0, reg))
  }

  def generateBrackets(inside: ExprNode): mutable.ListBuffer[Instruction] = generateExpressionOptimised(inside)

  def generateCharLiter(node: CharLiterNode): mutable.ListBuffer[Instruction] =
    mutable.ListBuffer(
      Move(node.register(), ImmValChar(node.x))
    )

  def generateBoolLiter(node: BoolLiterNode): mutable.ListBuffer[Instruction] =
    mutable.ListBuffer(
      Load(node.register(), LoadImmVal(if (node.x) 1 else 0))
    )

  def generateStrLiter(node: StrLiterNode): mutable.ListBuffer[Instruction] = {
    val label = Labels.addDataMsg(node.x)
    mutable.ListBuffer(
      Load(node.register(), LabelOp(label))
    )
  }

  def generateIntLiter(node: IntLiterNode): mutable.ListBuffer[Instruction] =
    mutable.ListBuffer(
      Load(node.register(), LoadImmVal(node.x))
    )

  def generatePairLiter(node: PairLiterNode): mutable.ListBuffer[Instruction] =
    mutable.ListBuffer(
      Load(node.register(), LoadImmVal(0))
    )

  def generateUnaryOperation(node: UnaryOperatorNode): mutable.ListBuffer[Instruction] = {

    val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
    node match {
      case NotNode(expr) =>
        instructions ++= generateExpressionOptimised(expr)
        instructions ++= List(
          Cmp(expr.register(), ImmVal(0)),
          Move(expr.register(), ImmVal(0), NE),
          Move(expr.register(), ImmVal(1), EQ),
        )
      case NegNode(expr) =>
        UtilityFunctions.arithmeticFlag = true
        instructions ++= generateExpressionOptimised(expr)
        instructions ++= List(
          ReverseSub(expr.register(), expr.register(), ImmVal(0)),
          BranchLink("p_throw_overflow_error", VS)
        )
      case ChrNode(expr) =>
        instructions ++= generateExpressionOptimised(expr)
      case OrdNode(expr) =>
        instructions ++= generateExpressionOptimised(expr)
      case LenNode(expr) =>
        instructions ++= generateExpressionOptimised(expr)
        instructions ++= List(
          Load(expr.register(), ShiftRegister(Offset(r0)))
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

    val leftReg = op.left.register()
    val rightReg = op.right.register()

    val binOpInstr: mutable.ListBuffer[Instruction] = op match {
      case _: AddNode =>
        mutable.ListBuffer(
          Add(leftReg, leftReg, rightReg, signed = true)
        )
      case _: SubNode =>
        mutable.ListBuffer(
          Sub(leftReg, leftReg, rightReg, signed = true)
        )
      case _: MulNode =>
        condForOverFlow = NE
        mutable.ListBuffer(
          SMull(leftReg, rightReg, leftReg, rightReg),
          Cmp(rightReg, ShiftRegister(PostIndex(leftReg, shift = Some(ASR), shiftVal = ImmVal(31))))
        )
      case _: DivNode =>
        UtilityFunctions.divisionFlag = true
        mutable.ListBuffer(
          Move(r0, leftReg),
          Move(r1, rightReg),
          BranchLink("p_check_divide_by_zero"),
          BranchLink("__aeabi_idiv"),
          Move(leftReg, r0)
        )
      case _: ModNode =>
        UtilityFunctions.divisionFlag = true
        mutable.ListBuffer(
          Move(r0, leftReg),
          Move(r1, rightReg),
          BranchLink("p_check_divide_by_zero"),
          BranchLink("__aeabi_idivmod"),
          Move(leftReg, r1)
        )
    }

    generateExpressionOptimised(op.left) ++
      generateExpressionOptimised(op.right) ++
      binOpInstr ++
      mutable.ListBuffer(BranchLink("p_throw_overflow_error", condForOverFlow), Move(r0, leftReg))
  }

  def generateBoolOperation(op: BoolBinaryOperator): mutable.ListBuffer[Instruction] = {

    val leftReg = op.left.register()
    val rightReg = op.right.register()

    val boolInstr: List[Instruction] = op match {
      case LANDNode(_, _) =>
        List(And(r0, leftReg, rightReg))
      case LORNode(_, _) =>
        List(Orr(r0, leftReg, rightReg))
    }

    generateExpressionOptimised(op.left) ++
      generateExpressionOptimised(op.right) ++
      boolInstr
  }

  def generateCompareOperation(op: IntCharBinaryOperator): mutable.ListBuffer[Instruction] = {
    val instructions: mutable.ListBuffer[Instruction] = op match {
      case GTNode(_, _) =>
        compOpBuilderOptimised.setCond(GT, LE).build(op)
      case GTENode(_, _) =>
        compOpBuilderOptimised.setCond(GE, LT).build(op)
      case LTNode(_, _) =>
        compOpBuilderOptimised.setCond(LT, GE).build(op)
      case LTENode(_, _) =>
        compOpBuilderOptimised.setCond(LE, GT).build(op)
    }

    instructions
  }

  def generateEqualOperation(op: TypelessBinaryOperator): mutable.ListBuffer[Instruction] = {

    val instructions: mutable.ListBuffer[Instruction] = op match {
      case EQNode(_, _) =>
        compOpBuilderOptimised.setCond(EQ, NE).build(op)
      case NEQNode(_, _) =>
        compOpBuilderOptimised.setCond(NE, EQ).build(op)
    }

    instructions
  }

  def generateExpressionOptimised(exprNode: ExprNode): mutable.ListBuffer[Instruction] = {
    exprNode match {
      case node: IdentNode => generateIdent(node)
      case x: ArrayElemNode => generateArrayElem(x)
      case x: ArrayLiterNode => generateArrayLiter(x)
      case BracketsNode(inside) => generateBrackets(inside)
      case x: CharLiterNode => generateCharLiter(x)
      case x: BoolLiterNode => generateBoolLiter(x)
      case x: StrLiterNode => generateStrLiter(x)
      case x: IntLiterNode => generateIntLiter(x)
      case x: PairLiterNode => generatePairLiter(x)
      case node: UnaryOperatorNode => generateUnaryOperation(node)
      case node: BinaryOperatorNode => generateBinaryOperation(node)
    }
  }
}
