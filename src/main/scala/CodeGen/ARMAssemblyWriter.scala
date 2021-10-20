package CodeGen

// Translates instructions into ARM THUMB format

class ARMAssemblyWriter() extends AssemblyWriter {

  def translateInstruction(instruction: Instruction): String =
    instruction match {
      case Add(dest, src, op, condition, signed) => s"\tADD${translateSigned(signed)}$condition ${translateRegister(dest)}, ${translateRegister(src)}, ${translateOperand(op)}"
      case Sub(dest, src, op, condition, signed) => s"\tSUB${translateSigned(signed)}$condition ${translateRegister(dest)}, ${translateRegister(src)}, ${translateOperand(op)}"
      case ReverseSub(dest, src, op, condition) => s"\tRSB$condition ${translateRegister(dest)}, ${translateRegister(src)}, ${translateOperand(op)}"
      case SMull(dest1, dest2, src1, src2, condition) => s"\tSMULL$condition ${translateRegister(dest1)}, ${translateRegister(dest2)}, ${translateRegister(src1)}, ${translateRegister(src2)}"
      case Div(dest, src, op, condition) => s"\tDIV$condition ${translateRegister(dest)}, ${translateRegister(src)}, ${translateOperand(op)}"
      case Load(dest, op, condition) => s"\tLDR$condition ${translateRegister(dest)}, ${translateOperand(op)}"
      case LoadSByte(dest, op, condition) => s"\tLDRSB$condition ${translateRegister(dest)}, ${translateOperand(op)}"
      case Store(src, dest, condition) => s"\tSTR$condition ${translateRegister(src)}, ${translateOperand(dest)}"
      case StoreByte(src, dest, condition) => s"\tSTRB$condition ${translateRegister(src)}, ${translateOperand(dest)}"
      case Move(dest, op, condition) => s"\tMOV$condition ${translateRegister(dest)}, ${translateOperand(op)}"
      case Branch(label, condition) => s"\tB$condition $label"
      case BranchLink(label, condition) => s"\tBL$condition $label"
      case And(dest, src, op, condition) => s"\tAND$condition ${translateRegister(dest)}, ${translateRegister(src)}, ${translateOperand(op)}"
      case Xor(dest, src, op, condition) => s"\tEOR$condition ${translateRegister(dest)}, ${translateRegister(src)}, ${translateOperand(op)}"
      case Orr(dest, src, op, condition) => s"\tORR$condition ${translateRegister(dest)}, ${translateRegister(src)}, ${translateOperand(op)}"
      case Cmp(src, op, condition) => s"\tCMP$condition ${translateRegister(src)}, ${translateOperand(op)}"
      case Push(src, condition) => s"\tPUSH$condition {${translateRegister(src)}}"
      case Pop(dest, condition) => s"\tPOP$condition {${translateRegister(dest)}}"
      case Label(labelName) => s"$labelName:"
      case Directive(name) => s".$name"
    }

  def translateSigned(signed: Boolean): String = if (signed) "S" else ""

  def translateAddressMode(addrMode: AddressMode): String = {
    addrMode match {
      case Offset(baseReg, reg, shift, shiftVal) =>
        val baseRegString = translateRegister(baseReg)
        var result = (shiftVal, shift, reg) match {
          case (ImmVal(0), _, _) => ""
          case (_, _, None) => s", ${translateOperand(shiftVal)}"
          case (_, Some(shift_), Some(reg_)) => s", ${translateRegister(reg_)}, $shift_ ${translateOperand(shiftVal)}"
          case (_, None, Some(reg_)) => s", ${translateRegister(reg_)}"
        }
        result = s"[$baseRegString$result]"
        result
      case PreIndex(baseReg, reg, shift, shiftVal) =>
        val baseRegString = translateRegister(baseReg)
        var result = (shiftVal, shift, reg) match {
          case (ImmVal(0), _, _) => ""
          case (_, _, None) => s", ${translateOperand(shiftVal)}"
          case (_, Some(shift_), Some(reg_)) => s", ${translateRegister(reg_)}, $shift_ ${translateOperand(shiftVal)}"
          case (_, None, Some(reg_)) => s", ${translateRegister(reg_)}"
        }
        result = s"[$baseRegString$result]!"
        result
      case PostIndex(baseReg, reg, shift, shiftVal) =>
        val baseRegString = translateRegister(baseReg)
        val result = (shiftVal, shift, reg) match {
          case (ImmVal(0), _, _) => ""
          case (_, None, None) => s", ${translateOperand(shiftVal)}"
          case (_, Some(shift_), None) => s", $shift_ ${translateOperand(shiftVal)}"
          case (_, Some(shift_), Some(reg_)) => s", ${translateRegister(reg_)}, $shift_ ${translateOperand(shiftVal)}"
          case (_, None, Some(reg_)) => s", ${translateRegister(reg_)}"
        }
        s"$baseRegString$result"
    }
  }

  def translateOperand(op: Operand): String =
    op match {
      case ImmVal(value) => s"#$value"
      case LoadImmVal(value) => s"=$value"
      case LabelOp(value) => s"=$value"
      case ImmValChar(value) => s"#\'$value\'"
      case ShiftRegister(value, reg) =>
        val result: String = (value, reg) match {
          case (Some(value_), None) => translateOperand(value_)
          case (None, Some(reg_)) => translateAddressMode(reg_)
          case _ => ""
        }
        result
      case reg: Register => translateRegister(reg)
    }

  def translateRegister(reg: Register): String =
    reg match {
      case StackPointer() => "sp"
      case LinkRegister() => "lr"
      case ProgramCounter() => "pc"
      case FramePointer() => "fp"
      case Reg(n) => s"r$n"
    }
}
