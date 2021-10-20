package CodeGen

import CodeGen.Condition._
import CodeGen.Constants._
import CodeGen.ErrorMsg.{divideByZero, overflow}
import CodeGen.UtilityFunctions.runtimeFlag

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object UtilityFunctions {

  // Flags indicating whether an utility function is needed
  var freePairFlag: Boolean = false
  var runtimeFlag: Boolean = false
  var nullPointerFlag: Boolean = false
  var readCharFlag: Boolean = false
  var readIntFlag: Boolean = false
  var printStringFlag: Boolean = false
  var printBoolFlag: Boolean = false
  var printIntFlag: Boolean = false
  var printCharFlag: Boolean = false
  var printRefFlag: Boolean = false
  var printLineFlag: Boolean = false
  var arithmeticFlag: Boolean = false
  var divisionFlag: Boolean = false
  var checkArrayBoundsFlag: Boolean = false

  // Labels used by utility functions
  val PRINT_STRING_LABEL = "p_print_string"
  val PRINT_INT_LABEL = "p_print_int"
  val PRINT_BOOL_LABEL = "p_print_bool"
  val PRINT_REF_LABEL = "p_print_reference"
  val PRINTLN_LABEL = "p_print_ln"
  val DIVIDE_BY_ZERO_LABEL = "p_check_divide_by_zero"
  val NULL_POINTER_LABEL = "p_check_null_pointer"
  val ARRAY_BOUNDS_LABEL = "p_check_array_bounds"
  val OVERFLOW_LABEL = "p_throw_overflow_error"
  val RUNTIME_LABEL = "p_throw_runtime_error"
  val READ_CHAR_LABEL = "p_read_char"
  val READ_INT_LABEL = "p_read_int"
  val FREE_PAIR_LABEL = "p_free_pair"

  def addUtilityFunctions()(implicit instructions: mutable.Buffer[Instruction]): Unit = {
    /* If flags for utility functions are set, add definitions at the end of the assembly code */
    if (UtilityFunctions.freePairFlag) {
      UtilityFunctions.runtimeFlag = true
      freePair()
    }
    if (UtilityFunctions.nullPointerFlag) {
      UtilityFunctions.runtimeFlag = true
      checkNullPointer()
    }
    if (UtilityFunctions.readCharFlag) {
      readChar()
    }
    if (UtilityFunctions.readIntFlag) {
      readInt()
    }
    if (UtilityFunctions.arithmeticFlag) {
      UtilityFunctions.runtimeFlag = true
      throwOverflowError()
    }
    if (UtilityFunctions.divisionFlag) {
      Labels.addDataMsgWithLabel(ErrorMsg.divideByZero, DIVIDE_BY_ZERO_LABEL)
      checkDivideByZero()
    }
    if (UtilityFunctions.checkArrayBoundsFlag) {
      Labels.addDataMsgWithLabel(ErrorMsg.arrayOutOfBoundsLo, "neg_index")
      Labels.addDataMsgWithLabel(ErrorMsg.arrayOutOfBoundsHi, "index_too_big")
      checkArrayBounds()
    }
    if (UtilityFunctions.runtimeFlag) {
      throwRuntimeError()

      if (!UtilityFunctions.printStringFlag) printString()

      if (!UtilityFunctions.printLineFlag) UtilityFunctions.printLineFlag = true
    }
    if (UtilityFunctions.printStringFlag) printString()

    if (UtilityFunctions.printBoolFlag) printBool()

    if (UtilityFunctions.printLineFlag) printLine()

    if (UtilityFunctions.printRefFlag) printRef()

    if (UtilityFunctions.printIntFlag) printInt()
  }

  /* Helpers to add a data msg and return the appropriate msg label for utility functions */

  def addOverFlowMsg(): String = {
    Labels.addDataMsgWithLabel(overflow, OVERFLOW_LABEL)
  }

  def addDivideByZeroMsg(): String = {
    Labels.addDataMsgWithLabel(divideByZero, DIVIDE_BY_ZERO_LABEL)
  }

  def addIntMsg(): String = {
    Labels.addDataMsg("%d\u0000")
  }

  def addCharMsg(): String = {
    Labels.addDataMsg(" %c\u0000")
  }

  def addStringMsg(): String = {
    Labels.addDataMsg("%.*s\u0000")
  }

  def addStringTerminatorMsg(): String = {
    Labels.addDataMsg("\u0000")
  }

  def addPrintTrueMsg(): String = {
    Labels.addDataMsg("true\u0000")
  }

  def addPrintFalseMsg(): String = {
    Labels.addDataMsg("false\u0000")
  }

  def addReferenceMsg(): String = {
    Labels.addDataMsg("%p\u0000")
  }

  def addNullCheckMsg(): String = {
    Labels.addDataMsg(ErrorMsg.nullReference)
  }

  def addNegIndexMsg(): String = {
    Labels.addDataMsg(ErrorMsg.arrayOutOfBoundsLo)
  }

  def addIndexTooBigMsg(): String = {
    Labels.addDataMsg(ErrorMsg.arrayOutOfBoundsHi)
  }

  /* Errors */
  def throwOverflowError()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Label(OVERFLOW_LABEL),
      Load(r0, LabelOp(addOverFlowMsg())),
      BranchLink(RUNTIME_LABEL))
  }

  def throwRuntimeError()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Label(RUNTIME_LABEL),
      BranchLink(PRINT_STRING_LABEL),
      Move(r0, ImmVal(-1)),
      BranchLink("exit"))
  }

  /* Checks */
  def checkDivideByZero()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Label(DIVIDE_BY_ZERO_LABEL),
      Push(lr),
      Cmp(r1, ImmVal(0)),
      Load(r0, LabelOp(addDivideByZeroMsg()), EQ),
      BranchLink(RUNTIME_LABEL, EQ),
      Pop(pc))
  }

  def checkNullPointer()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Label(NULL_POINTER_LABEL),
      Push(lr),
      Cmp(r0, ImmVal(0)),
      Load(r0, LabelOp(addNullCheckMsg()), EQ),
      BranchLink(RUNTIME_LABEL, EQ),
      Pop(pc))
  }

  def checkArrayBounds()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    runtimeFlag = true

    instrList ++= List(
      Label(ARRAY_BOUNDS_LABEL),
      Push(lr),
      Cmp(r0, ImmVal(0)),
      Load(r0, LabelOp(addNegIndexMsg()), LT),
      BranchLink(RUNTIME_LABEL, LT),
      Load(r1, ShiftRegister(Offset(r1))),
      Cmp(r0, r1),
      Load(r0, LabelOp(addIndexTooBigMsg()), CS),
      BranchLink(RUNTIME_LABEL, CS),
      Pop(pc))
  }

  /* End print messages */
  private def endPrint()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Add(r0, r0, ImmVal(WORD_SIZE)),
      BranchLink("printf"),
      Move(r0, ImmVal(0)),
      BranchLink("fflush"),
      Pop(pc))
  }

  /* Print */
  def printInt()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Label(PRINT_INT_LABEL),
      Push(lr),
      Move(r1, r0),
      Load(r0, LabelOp(addIntMsg())))
    endPrint()
  }

  def printBool()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Label(PRINT_BOOL_LABEL),
      Push(lr),
      Cmp(r0, ImmVal(0)),
      Load(r0, LabelOp(addPrintTrueMsg()), NE),
      Load(r0, LabelOp(addPrintFalseMsg()), EQ))
    endPrint()
  }

  def printString()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Label(PRINT_STRING_LABEL),
      Push(lr),
      Load(r1, ShiftRegister(Offset(r0))),
      Add(r2, r0, ImmVal(WORD_SIZE)),
      Load(r0, LabelOp(addStringMsg())))
    endPrint()
  }

  def printLine()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Label(PRINTLN_LABEL),
      Push(lr),
      Load(r0, LabelOp(addStringTerminatorMsg())),
      Add(r0, r0, ImmVal(WORD_SIZE)),
      BranchLink("puts"),
      Move(r0, ImmVal(0)),
      BranchLink("fflush"),
      Pop(pc)
    )
  }

  def printRef()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Label(PRINT_REF_LABEL),
      Push(lr),
      Move(r1, r0),
      Load(r0, LabelOp(addReferenceMsg())))
    endPrint()
  }

  /* End read messages */
  private def endRead()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Add(r0, r0, ImmVal(WORD_SIZE)),
      BranchLink("scanf"),
      Pop(pc))
  }

  /* Read */
  def readChar()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Label(READ_CHAR_LABEL),
      Push(lr),
      Move(r1, r0),
      Load(r0, LabelOp(addCharMsg())))
    endRead()
  }

  def readInt()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Label(READ_INT_LABEL),
      Push(lr),
      Move(r1, r0),
      Load(r0, LabelOp(addIntMsg())))
    endRead()
  }


  /* Free pair */
  def freePair()(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    instrList ++= List(
      Label(FREE_PAIR_LABEL),
      Push(lr),
      Cmp(r0, ImmVal(0)),
      Load(r0, LabelOp(addNullCheckMsg()), EQ),
      Branch(RUNTIME_LABEL, EQ),
      Push(r0),
      Load(r0, ShiftRegister(Offset(r0))),
      BranchLink("free"),
      Load(r0, ShiftRegister(Offset(sp))),
      Load(r0, ShiftRegister(Offset(r0, shiftVal = ImmVal(WORD_SIZE)))),
      BranchLink("free"),
      Pop(r0),
      BranchLink("free"),
      Pop(pc))
  }

  def sizeOfDataType(t: String, isPair: Boolean=false, pos: Int=0): Int = {
    if (isPair) {
      val x = pos match {
        case 1 =>
          sizeOfDataType(t.substring(t.indexOf("(") + 1, t.indexOf(",")))
        case 2 => sizeOfDataType(t.substring(t.indexOf(",") + 1, t.lastIndexOf(")")))
      }
      println(x)
    }

    t match {
      case "bool" | "char" => BYTE_SIZE
      case _ => WORD_SIZE
    }
  }
}
