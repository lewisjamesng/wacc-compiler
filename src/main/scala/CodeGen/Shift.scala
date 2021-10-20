package CodeGen

object Shift extends Enumeration {
  type Shift = Value
  val LSL = Value("LSL")      // Logical shift left
  val LSR = Value("LSR")      // Logical shift right
  val ASR = Value("ASR")      // Arithmetic shift right
  val ROR = Value("ROR")      // Rotate right
}
