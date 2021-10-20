package CodeGen

import CodeGen.Condition._
import CodeGen.Constants.{r0, r4, r5}
import CodeGen.GenerateExpressions.generateExpression
import LexerSemanticCheck.BinaryOperatorNode

import scala.collection.mutable

object compOpBuilderOptimised extends operationBuilder {
  private var cond1: Condition = _
  private var cond2: Condition = _

  def setCond(cond1: Condition, cond2: Condition): operationBuilder = {
    this.cond1 = cond1
    this.cond2 = cond2
    this
  }

  def build(op: BinaryOperatorNode): mutable.ListBuffer[Instruction] = {

    val leftReg = op.left.register()
    val rightReg = op.right.register()

    generateExpression(op.left) ++
      generateExpression(op.right) ++
      List(Cmp(leftReg, rightReg)) ++
      List(Load(r0, LoadImmVal(1), cond1),
        Load(r0, LoadImmVal(0), cond2))
  }
}