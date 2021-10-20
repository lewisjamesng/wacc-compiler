package CodeGen

object Condition extends Enumeration {
  type Condition = Value
  val EQ, NE, CS, CC, MI, PL, VS, VC, HI, LS, LT, GT, LE, GE = Value
  val AL: Value = Value("")
}
/*
  EQ:     Equal
  NE:     Not equal
  CS/HS:  Unsigned higher or same, carry set
  CC/L0:  Unsigned lower, Carry clear (identical to LO)
  MI:     Negative, minus
  PL:     Positive or zero, plus
  VS:     Overflow
  VC:     No overflow
  HI:     (Unsigned) higher
  LS:     (Unsigned) lower or same
  GE:     (Signed) greater than or equal
  LT:     (Signed) less than
  GT:     (Signed) greater than
  LE:     (Signed) less than or equal
  AL:     Always (default)
*/