package CodeGen

import CodeGen.Condition.{AL, Condition}

sealed trait Instruction

/* Arithmetic Instructions */
case class Add(dest: Register, src: Register, op: Operand, condition: Condition = AL, signed: Boolean=false) extends Instruction

case class Sub(dest: Register, src: Register, op: Operand, condition: Condition = AL, signed: Boolean=false) extends Instruction

case class ReverseSub(dest: Register, src: Register, op: Operand, condition: Condition = AL) extends Instruction

case class SMull( dest1: Register, dest2: Register, src1: Register, src2: Register, condition: Condition = AL) extends Instruction

case class Div(dest: Register, src: Register, op: Operand, condition: Condition = AL) extends Instruction

/* Load + Store Instructions */
case class Load(dest: Register, op: Operand, condition: Condition = AL) extends Instruction

case class LoadSByte(dest: Register, op: Operand, condition: Condition = AL) extends Instruction

case class Store(src: Register, dest: Operand, condition: Condition = AL) extends Instruction

case class StoreByte(src: Register, dest:Operand, condition: Condition = AL) extends Instruction

case class Move(dest: Register, op: Operand, condition: Condition = AL) extends Instruction

/* Branch Instructions */
case class Branch(label: String, condition: Condition = AL) extends Instruction

case class BranchLink(dstLabel: String, condition: Condition = AL) extends Instruction

/* Logical Instructions */
case class And(dest: Register, src: Register, op: Operand, condition: Condition = AL) extends Instruction

case class Xor(dest: Register, src: Register, op: Operand, condition: Condition = AL) extends Instruction

case class Orr(dest: Register, src: Register, op: Operand, condition: Condition = AL) extends Instruction

/* Compare CodeGen.Instruction */
case class Cmp(src: Register, op: Operand, condition: Condition = AL) extends Instruction

/* Stack CodeGen.Push & CodeGen.Pop Instructions */
case class Push( src: Register, condition: Condition = AL) extends Instruction

case class Pop(dest: Register, condition: Condition = AL) extends Instruction

/* Label Instruction */
case class Label(labelName: String) extends Instruction

case class Directive(name: String) extends Instruction