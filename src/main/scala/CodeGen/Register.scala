package CodeGen

trait Register extends Operand

case class StackPointer() extends Register

case class LinkRegister() extends Register

case class ProgramCounter() extends Register

case class FramePointer() extends Register

case class Reg(n: Int) extends Register
