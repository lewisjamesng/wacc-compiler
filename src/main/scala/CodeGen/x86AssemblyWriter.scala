package CodeGen

import CodeGen.Condition._
import CodeGen.Shift._

// Translates instructions into x86 assembly.

class x86AssemblyWriter() extends AssemblyWriter {
  var bypassCounter = 0
  var writeBackFlag = false
  var writeToReg = ""
  var writeFromReg = ""
  var scale = 0

  def createCondSections(condition: Condition): (String, String) = {
    var condSection1 = ""
    var condSection2 = ""
    if (condition != AL) {
      condSection1 = translateInstruction(Branch(s"BYPASS$bypassCounter\n", flipCond(condition)))
      condSection2 = translateInstruction(Label(s"\nBYPASS$bypassCounter"))
      bypassCounter += 1
    }
    if (writeBackFlag) {
      writeFromReg match {
        case "" => condSection2 = s"$condSection2\n\tADD $scale, $writeToReg"
        case _  => condSection2 = s"$condSection2\n\tMOV $writeFromReg, %r8d\n\tMUL $scale, %r8\n\tADD %r8, writeToReg"
      }
    }
    (condSection1, condSection2)
  }

  def translateInstruction(instruction: Instruction): String = {
    instruction match {
      case Add(dest, src, op, condition, _) =>
        val (destination, source, operand) = (translateRegister(dest), translateRegister(src), translateOperand(op))
        val base = s"\tMOV $source, $destination\n\tADD $operand, $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case Sub(dest, src, op, condition, _) =>
        val (destination, source, operand) = (translateRegister(dest), translateRegister(src), translateOperand(op))
        val base = s"\tMOV $source, $destination\n\tSUB $operand, $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case ReverseSub(dest, src, op, condition) =>
        val (destination, source, operand) = (translateRegister(dest), translateRegister(src), translateOperand(op))
        val base = s"\tMOV $operand, $destination\n\tSUB $source, $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2

      case SMull(dest1, _, src1, src2, condition) =>
        val (destination, source1, source2) = (translateRegister(dest1), translateRegister(src1), translateRegister(src2))
        val base = s"\tMOV $source1, $destination\n\tIMULD $source2, $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case Div(dest, src, op, condition) =>
        val (destination, source, operand) = (translateRegister(dest), translateRegister(src), translateOperand(op))
        val base = s"\tMOV $source, $destination\n\tIDIVD $operand, $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case Load(dest, op, condition) => translateInstruction(Move(dest, op, condition))
      case LoadSByte(dest, op, condition) =>
        val (destination, operand) = (translateRegister(dest), translateOperand(op))
        val base = s"\tMOVSXB $operand, $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case Store(src, dest, condition) =>
        val (source, destination) = (translateRegister(src), translateOperand(dest))
        val base = s"\tMOV $source, $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case StoreByte(src, dest, condition) =>
        val (source, destination) = (translateRegister(src), translateOperand(dest))
        val base = s"\tMOVB $source, $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case Move(dest, op, condition) =>
        val (destination, operand) = (translateRegister(dest), translateOperand(op))
        val base = s"\tMOV $operand, $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case Branch(label, condition) => s"\tJ${translateCondition(condition)} $label"
      case BranchLink(label, condition) =>
        var base = ""
        var condSections = ("", "")
        if (label != "__aeabi_idiv" && label != "__aeabi_idivmod") {
          base = s"\tCALL $label"
          condSections = createCondSections(condition)
        }

        condSections._1 + base + condSections._2
      case And(dest, src, op, condition) =>
        val (destination, source, operand) = (translateRegister(dest), translateRegister(src), translateOperand(op))
        val base = s"\tMOV $source, $destination\n\tAND $operand, $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case Xor(dest, src, op, condition) =>
        val (destination, source, operand) = (translateRegister(dest), translateRegister(src), translateOperand(op))
        val base = s"\tMOV $source, $destination\n\tXOR $operand, $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case Orr(dest, src, op, condition) =>
        val (destination, source, operand) = (translateRegister(dest), translateRegister(src), translateOperand(op))
        val base = s"\tMOV $source, $destination\n\tOR $operand, $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case Cmp(src, op, condition) =>
        val (source, operand) = (translateRegister(src), translateOperand(op))
        val base = s"\tCMP $operand, $source"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case Push(src, condition) =>
        val source = translateRegister(src)
        val base = s"\tPUSH $source"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case Pop(dest, condition) =>
        val destination = translateRegister(dest)
        val base = s"\tPOP $destination"
        val condSections = createCondSections(condition)

        condSections._1 + base + condSections._2
      case Label(labelName) => s"$labelName:"
      case Directive(name) =>
        name match {
          case "ltorg" => "\tINT $0x80"
          case _        => s".$name"
        }
    }
  }

  def translateCondition(cond: Condition): String =
    cond match {
      case EQ => "E"
      case NE => "NE"
      case CS => "AE"
      case CC => "B"
      case MI => "S"
      case PL => "NS"
      case VS => "O"
      case VC => "NO"
      case HI => "A"
      case LS => "BE"
      case GE => "GE"
      case LT => "L"
      case GT => "G"
      case LE => "LE"
      case _ => "MP"
    }

  def flipCond(cond: Condition): Condition =
    cond match {
      case EQ => NE
      case NE => EQ
      case CS => CC
      case CC => CS
      case MI => PL
      case PL => MI
      case VS => VC
      case VC => VS
      case HI => LS
      case LS => HI
      case GE => LT
      case LT => GE
      case GT => LE
      case LE => GT
    }

  def translateAddressMode(addrMode: AddressMode): String =
    addrMode match {
      case Offset(baseReg, reg, shift, shiftVal) =>
        var shiftNum = 0
        val baseRegString = translateRegister(baseReg)
        val addressing = (shiftVal, shift, reg) match {
          case (ImmVal(0), _, _) => ("", "")
          case (_, _, None) => (s"${translateOperand(shiftVal)}", "")
          case (_, Some(shift_), Some(reg_)) =>
            val n = translateOperand(shiftVal).substring(1).toInt
            val addressingReg = translateRegister(reg_)
            shift_ match {
              case LSL => ("", s", $addressingReg, ${n << 1}")
              case LSR => ("", s", $addressingReg, ${n >> 1}")
              case ASR =>
                if (n >> 31 == 1) {
                  shiftNum = (n >> 1) + (1 << 31)
                } else {
                  shiftNum = n >> 1
                }
                ("", s", $addressingReg, $shiftNum")
              case ROR =>
                if (n % 2 == 1) {
                  shiftNum = (n >> 1) + (1 << 31)
                } else {
                  shiftNum = n >> 1
                }
                ("", s", $addressingReg, $shiftNum")
            }
          case (_, None, Some(reg_)) => ("", s", ${translateRegister(reg_)}, 1")
        }
        s"${addressing._1}($baseRegString${addressing._2})"
      case PreIndex(baseReg, reg, shift, shiftVal) =>
        writeBackFlag = true
        var shiftNum = 0
        val baseRegString = translateRegister(baseReg)
        writeToReg = baseRegString
        val addressing = (shiftVal, shift, reg) match {
          case (ImmVal(0), _, _) => ("", "")
          case (_, _, None) =>
            scale = translateOperand(shiftVal).substring(1).toInt
            (s"${translateOperand(shiftVal)}", "")
          case (_, Some(shift_), Some(reg_)) =>
            shift_ match {
              case LSL =>
                writeFromReg = translateRegister(reg_)
                scale = translateOperand(shiftVal).substring(1).toInt << 1
                ("", s", $writeFromReg, $scale")
              case LSR =>
                writeFromReg = translateRegister(reg_)
                scale = translateOperand(shiftVal).substring(1).toInt >> 1
                ("", s", $writeFromReg, $scale")
              case ASR =>
                writeFromReg = translateRegister(reg_)
                val n = translateOperand(shiftVal).substring(1).toInt
                if (n >> 31 == 1) {
                  shiftNum = (n >> 1) + (1 << 31)
                } else {
                  shiftNum = n >> 1
                }
                scale = shiftNum
                ("", s", $writeFromReg, $shiftNum")
              case ROR =>
                writeFromReg = translateRegister(reg_)
                val n = translateOperand(shiftVal).substring(1).toInt
                if (n % 2 == 1) {
                  shiftNum = (n >> 1) + (1 << 31)
                } else {
                  shiftNum = n >> 1
                }
                scale = shiftNum
                ("", s", $writeFromReg, $shiftNum")
            }
          case (_, None, Some(reg_)) =>
            writeFromReg = translateRegister(reg_)
            ("", s", $writeFromReg, 1")
        }
        s"${addressing._1}($baseRegString${addressing._2})"
      case PostIndex(baseReg, reg, shift, shiftVal) =>
        writeBackFlag = true
        var shiftNum = 0
        val baseRegString = translateRegister(baseReg)
        (shiftVal, shift, reg) match {
          case (_, _, None) => scale = translateOperand(shiftVal).substring(1).toInt
          case (_, Some(shift_), Some(reg_)) =>
            writeFromReg = translateRegister(reg_)
            shift_ match {
              case LSL => scale = translateOperand(shiftVal).substring(1).toInt << 1
              case LSR => scale = translateOperand(shiftVal).substring(1).toInt >> 1
              case ASR =>
                val n = translateOperand(shiftVal).substring(1).toInt
                if (n >> 31 == 1) {
                  shiftNum = (n >> 1) + (1 << 31)
                } else {
                  shiftNum = n >> 1
                }
                scale = shiftNum
              case ROR =>
                val n = translateOperand(shiftVal).substring(1).toInt
                if (n % 2 == 1) {
                  shiftNum = (n >> 1) + (1 << 31)
                } else {
                  shiftNum = n >> 1
                }
                scale = shiftNum
            }
          case (_, None, Some(reg_)) =>
            writeFromReg = translateRegister(reg_)
        }
        s"($baseRegString)"
    }

  def translateOperand(op: Operand): String =
    op match {
      case ImmVal(value) => s"$$$value"
      case LoadImmVal(value) => s"$$$value"
      case LabelOp(value) => s"$$$value"
      case ImmValChar(value) => s"$$${value.toInt}"
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
      case StackPointer() => "%rsp"
      case LinkRegister() => "%rax"
      case ProgramCounter() => "%rbx"
      case FramePointer() => "%rbp"
      case Reg(0) => "%rdi"
      case Reg(1) => "%rsi"
      case Reg(2) => "%rdx"
      case Reg(3) => "%rcx"
      case Reg(n) => s"%r${n + 4}"
    }
}