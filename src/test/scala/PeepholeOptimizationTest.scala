import CodeGen.{ImmVal, Instruction, Load, Pop, Push, Store}
import CodeGen.Constants._
import Optimisation.PeepholeOptimization.runPeepholeOptimization
import org.scalatest.Assertions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.{be, not}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable

class PeepholeOptimizationTest extends AnyFlatSpec with Assertions {

  "The peephole optimization tester" should "reduce redundant stack instructions" in {
    implicit val instructions: mutable.Buffer[Instruction] =
      mutable.Buffer(Pop(r3), Pop(r2), Pop(r1), Pop(r0), Push(r0), Push(r1), Push(r2), Push(r3))
    runPeepholeOptimization()
    instructions should be(mutable.Buffer.empty)
  }

  it should "not reduce redundant stack instructions if the registers are different" in {
    implicit val instructions: mutable.Buffer[Instruction] =
      mutable.Buffer(Pop(r3), Pop(r2), Pop(r1), Pop(r0), Push(r1), Push(r1), Push(r2), Push(r3))
    runPeepholeOptimization()
    instructions should not be mutable.Buffer.empty
  }

  it should "reduce redundant stores and loads with store first" in {
    implicit val instructions: mutable.Buffer[Instruction] = {
      mutable.Buffer(Store(r0, r1), Load(r1, r0))
    }
    runPeepholeOptimization()
    instructions should be(mutable.Buffer(Store(r0, r1)))
  }

  it should "reduce redundant stores and loads with load first" in {
    implicit val instructions: mutable.Buffer[Instruction] = {
      mutable.Buffer(Load(r1, r0), Store(r0, r1))
    }
    runPeepholeOptimization()
    instructions should be(mutable.Buffer(Load(r1, r0)))
  }

  it should "not reduce redundant stores and loads with different registers" in {
    implicit val instructions: mutable.Buffer[Instruction] = {
      mutable.Buffer(Load(r1, r0), Store(r0, r3))
    }
    runPeepholeOptimization()
    instructions should be(mutable.Buffer(Load(r1, r0), Store(r0, r3)))
  }

  it should "remove redundant load chains" in {
    implicit val instructions: mutable.Buffer[Instruction] = {
      mutable.Buffer(Load(r0, ImmVal(1)), Load(r0, ImmVal(1)), Load(r0, ImmVal(1)))
    }
    runPeepholeOptimization()
    instructions should be(mutable.Buffer(Load(r0, ImmVal(1))))
  }

  it should "remove redundant store chains" in {
    implicit val instructions: mutable.Buffer[Instruction] = {
      mutable.Buffer(Store(r3, r1), Store(r2, r1), Store(r0, r1))
    }
    runPeepholeOptimization()
    instructions should be(mutable.Buffer(Store(r0, r1)))
  }

  it should "reduce combinations of the redundant instructions" in {
    implicit val instructions: mutable.Buffer[Instruction] = {
      mutable.Buffer(Store(r3, r1), Store(r2, r1), Store(r0, r1),
        Load(r1, r2), Load(r1, r3), Load(r1, r0))
    }
    runPeepholeOptimization()
    instructions should be(mutable.Buffer(Store(r0, r1)))
  }
  
}
