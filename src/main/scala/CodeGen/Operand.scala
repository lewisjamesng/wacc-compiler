package CodeGen

trait Operand

/* Immediate Value */
case class ImmVal(value: Int) extends Operand

/* Load Immediate Value */
case class LoadImmVal(value: Int) extends Operand

case class LabelOp(value: String) extends Operand

case class ImmValChar(value: Char) extends Operand

case class ShiftRegister private(value: Option[ImmVal], reg: Option[AddressMode]) extends Operand {
  assert(value.isEmpty != reg.isEmpty)
}
object ShiftRegister {
  def apply(value: ImmVal): ShiftRegister = ShiftRegister(Some(value), None)
  def apply(reg: AddressMode): ShiftRegister = ShiftRegister(None, Some(reg))
}
