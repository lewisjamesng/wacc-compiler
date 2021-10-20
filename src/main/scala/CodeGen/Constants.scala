package CodeGen

/* Store constants such as registers */
object Constants {

  val BYTE_SIZE: Int = 1
  val CHAR_SIZE: Int = 1
  val WORD_SIZE: Int = 4
  val PAIR_SIZE: Int = WORD_SIZE * 2
  val EOF: String = "\n"
  val MAX_SP_SIZE: Int = 1024

  /* General Purpose Registers */
  val r0: Reg = Reg(0)
  val r1: Reg = Reg(1)
  val r2: Reg = Reg(2)
  val r3: Reg = Reg(3)
  val r4: Reg = Reg(4)
  val r5: Reg = Reg(5)
  val r6: Reg = Reg(6)
  val r7: Reg = Reg(7)

  /* Special Purpose Registers */
  val fp: FramePointer = FramePointer()
  val sp: StackPointer = StackPointer()
  val lr: LinkRegister = LinkRegister()
  val pc: ProgramCounter = ProgramCounter()
}
