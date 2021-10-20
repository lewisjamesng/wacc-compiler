package CodeGen
import CodeGen.Shift._

sealed trait AddressMode {
  /* Return the address of the register shifted by the given value */
  def addToShiftVal(value: Int): AddressMode
}

case class Offset(baseReg: Register, reg: Option[Register] = None, shift: Option[Shift] = None,
                  shiftVal: ImmVal = ImmVal(0)) extends AddressMode {
  override def addToShiftVal(value: Int): Offset = Offset(baseReg, reg, shift, ImmVal(shiftVal.value + value))
}

case class PreIndex(baseReg: Register, reg: Option[Register] = None, shift: Option[Shift] = None,
                    shiftVal: ImmVal = ImmVal(0)) extends AddressMode {
  override def addToShiftVal(value: Int): PreIndex = PreIndex(baseReg, reg, shift, ImmVal(shiftVal.value + value))
}

case class PostIndex(baseReg: Register, reg: Option[Register] = None, shift: Option[Shift] = None,
                     shiftVal: ImmVal = ImmVal(0)) extends AddressMode {
  override def addToShiftVal(value: Int): PostIndex = PostIndex(baseReg, reg, shift, ImmVal(shiftVal.value + value))
}

