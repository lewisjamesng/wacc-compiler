package CodeGen

import CodeGen.Condition._
import CodeGen.Constants.{r0, r4, r5}
import CodeGen.GenerateExpressions.generateExpression
import LexerSemanticCheck.BinaryOperatorNode

import scala.collection.mutable

trait Builder

abstract class operationBuilder() {
  def setCond(cond1: Condition, cond2: Condition) : operationBuilder

  def build(op: BinaryOperatorNode) : mutable.ListBuffer[Instruction]
}

object compOpBuilder extends operationBuilder {
  private var cond1: Condition = _
  private var cond2: Condition = _

  def setCond(cond1: Condition, cond2: Condition): operationBuilder = {
    this.cond1 = cond1
    this.cond2 = cond2
    this
  }

  def build(op: BinaryOperatorNode): mutable.ListBuffer[Instruction] = {
    generateExpression(op.left) ++
      List(Push(r0)) ++
      generateExpression(op.right) ++
      List(Move(r5, r0), Pop(r4), Cmp(r4, r5)) ++
      List(Load(r0, LoadImmVal(1), cond1),
        Load(r0, LoadImmVal(0), cond2))
  }
}
