package CodeGen

import CodeGen.Constants._
import Optimisation.PeepholeOptimization.runPeepholeOptimization
import LexerSemanticCheck.{FuncNode, ProgramNode}
import UtilityFunctions._

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable

class CodeGenerator(assemblyWriter: AssemblyWriter) {

  def generateCode(p: ProgramNode, optimised: Boolean=false): mutable.Buffer[Instruction] = {

    // If there is no symbol table in program node should crash
    val rootSymbolTable = p.symbolTable match {
      case Some(value) => value
      case None => throw new Exception("Program root node has no associated symbol table!")
    }

    // Generate function reference table (name -> funcNode)
    FunctionMap.funcRef = (for (f <- p.funcList) yield f.i.varName -> f).toMap

    // This must be executed before getting instructions to populate the
    // data msg table

    implicit val instructions: mutable.Buffer[Instruction] = new mutable.ListBuffer[Instruction].empty

    // Sets up the code to run main and adjusts the stack pointer
    // to allocate space for variables
    instructions ++= List(
      Directive("text"),
      Directive("global main"),
      Label("main"),
      Push(lr))
    instructions ++= StackSimulation.addStackFrame(rootSymbolTable)

    // Adds the body of the main function
    if (!optimised) {
      instructions ++= GenerateStatements.generateStatement(p.stat, "")
    } else {
      instructions ++= GenerateStatementsOptimised.generateStatementOptimised(p.stat, "")
    }

    // Correctly scopes out of the main function and reallocates
    // space for destroyed local variables
    instructions ++= StackSimulation.freeStackFrame()
    instructions ++= List(
      Load(r0, LoadImmVal(0)),
      Pop(pc),
      Directive("ltorg"))

    addUtilityFunctions()

    instructions ++= FunctionMap.getAllUserFunctions.flatten

    if (Labels.dataMsgStore.nonEmpty) {
      // Adds all of the strings defined in the code to the data msg
      // section
      //Labels.dataMsgStore.flatMap(_.instruction) ++=: instructions
      Labels.dataMsgStore.flatMap(kv => kv._2.instruction) ++=: instructions
      // Start of data section
      Directive("data") +=: instructions
    }


    runPeepholeOptimization()

    instructions
  }

  def writeToFile(f: String)(implicit instructions: mutable.Buffer[Instruction]): Unit = {
    val bw = new BufferedWriter(new FileWriter(new File(f)))
    bw.write(assemblyWriter.translate(instructions).mkString("\n") + EOF)
    bw.close()
  }
}
