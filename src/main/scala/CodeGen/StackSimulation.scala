package CodeGen

import CodeGen.Constants._
import LexerSemanticCheck.{ParamListNode, SymbolTable}

import scala.collection.mutable.ListBuffer

object StackSimulation {

  private var stackFrames: List[StackFrame] = List().empty

  def declareVariableOnStack(s: String): Unit = {
    stackFrames.last.declaredVars += s
  }

  def printDeclaredVariables(): Unit = {
    println(stackFrames.last.declaredVars)
  }

  def topStackFrameSize(): Int = stackFrames.last.localVarSize

  def printStackFrame() : Unit = {
    println()
    println("\nPrinting Stack Frames")
    for (i <- stackFrames.indices) {
      println(s"$i-----------------------------")
      for ((k, v) <- stackFrames(i).varMap) {
        println(s"$k -> $v")
      }
      println(s"$i-----------------------------")
    }
  }

  def addStackFrame(symbolTable: SymbolTable, paramList: Option[ParamListNode] = None): ListBuffer[Instruction] = {
    val result = new ListBuffer[Instruction]().empty
    val newStackFrame = new StackFrame(symbolTable, paramList)
    stackFrames :+= newStackFrame

    val Instr = new ListBuffer[Instruction]().empty
    // 1: Push the frame pointer
    // 2: Allocate space for the local variables on the stack
    // 3: Set the fp to current sp

    result ++= List(Push(fp))
    result ++= allocateStackSpace(newStackFrame.localVarSize)
    result ++ List(Move(fp, sp))
  }

  def allocateStackSpace(size: Int): ListBuffer[Instruction] = {
    val subStackInstr = new ListBuffer[Instruction]().empty

    // no need to allocate if there are no variables
    if (size == 0) return subStackInstr

    var stackFrameSize = size
    while(stackFrameSize > MAX_SP_SIZE) {
      subStackInstr += Sub(sp, sp, ImmVal(MAX_SP_SIZE))
      stackFrameSize -= MAX_SP_SIZE
    }
    subStackInstr += Sub(sp, sp, ImmVal(stackFrameSize))
  }

  def freeStackFrame(isFunction: Boolean = false): ListBuffer[Instruction] = {
    val result = new ListBuffer[Instruction]().empty
    val removedFrame = stackFrames.last
    stackFrames = stackFrames.dropRight(1)

    if (!isFunction) {
      result ++= deallocateStackSpace(removedFrame.localVarSize)
    }
    result ++ List(Pop(fp))
  }

  def deallocateStackSpace(size: Int): ListBuffer[Instruction] = {
    // When exiting a stack frame you must:
    // 1: Deallocate space for the local variables
    // 2: Pop the old frame pointer to the frame pointer register
    // 3: Pop the old link register value to pc

    // Need multiple removes if stack frame is larger than 1024
    val addStackInstr = new ListBuffer[Instruction]().empty

    // no need to deallocate if there was none allocated
    if (size == 0) return addStackInstr

    var stackFrameSize = size
    while(stackFrameSize > MAX_SP_SIZE) {
      addStackInstr += Add(sp, sp, ImmVal(MAX_SP_SIZE))
      stackFrameSize -= MAX_SP_SIZE
    }
    addStackInstr += Add(sp, sp, ImmVal(stackFrameSize))
  }


  def offset(name: String) : Int = {
    // size of stack frames previous to the one where name is located
    var prevScopeSizes = 0
    // the offset in bytes
    var result = 0
    // whether the variable is in scope
    var varFound = false

    for (sF <- stackFrames.reverse) {
      if (!varFound) {
        sF.findVar(name) match {
          case Some(x) =>
             varFound = true
            result = x
          case None =>
        }
      }
      if (!varFound) {
        if(sF.isFunction) {
          // goes past all variables and params on stack frame
          // extra 2* WORD_SIZE is for the push(lr) and push(fp) per stack frame
          prevScopeSizes += sF.pushedArgsSize + sF.localVarSize + 2 * WORD_SIZE
        } else {
          // goes past all variables on stack frame
          // extra WORD_SIZE is for the push(fp) per stack frame
          prevScopeSizes += sF.localVarSize + WORD_SIZE

        }
      }
    }
    if (!varFound) {
      throw new Exception(s"Couldn't find variable \'$name\' on stack!")
    }
    prevScopeSizes + result
  }
}
