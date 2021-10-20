package CodeGen

import scala.collection.mutable

trait AssemblyWriter {

  /* Uses concrete implementation to translate a buffer of instructions into a string */
  def translate(instrList: mutable.Buffer[Instruction]): mutable.Buffer[String] = {
    instrList.map(translateInstruction)
  }

  /* Each concrete assembly writer must define how to translate an instruction into assembly */
  def translateInstruction(instruction: Instruction): String
}
