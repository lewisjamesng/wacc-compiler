package Optimisation

import CodeGen._

import scala.collection.mutable

object PeepholeOptimization {

  def runPeepholeOptimization()(implicit
      instructions: mutable.Buffer[Instruction]
  ): Unit = {
    removeRedundantLoads(0)
    removeRedundantStores(0)
    simplifyStackInstructions(0)
    removeRedundantStoresAndLoadPairs()
  }

  /*
     Storing and loading between the between the two same registers makes the
     second instruction redundant, so we can remove the second instruction
   */
  def removeRedundantStoresAndLoadPairs()(implicit
      instrList: mutable.Buffer[Instruction]
  ): Unit = {
    for ((instr, i) <- instrList.zipWithIndex) {
      instr match {
        case Store(s1, d1, c1) =>
          if (indexIsInRange(i + 1, instrList.size)) {
            instrList(i + 1) match {
              case Load(d2, s2, c2) =>
                if (s1.equals(s2) && d1.equals(d2) && c1.equals(c2)) {
                  instrList.remove(i + 1)
                }
              case _ =>
            }
          }
        case Load(d1, s1, c1) =>
          if (indexIsInRange(i + 1, instrList.size)) {
            instrList(i + 1) match {
              case Store(s2, d2, c2) =>
                if (s1.equals(s2) && d1.equals(d2) && c1.equals(c2)) {
                  instrList.remove(i + 1)
                }
              case _ =>
            }
          }
        case _ =>
      }
    }
  }

  /*
    PUSH rX
    POP rX

    POP rX
    PUSH rX

    does nothing overall so we can remove both and then recurse backwards for some case like

    POP r1
    POP r2
    PUSH r2
    PUSH r1
   */
  def simplifyStackInstructions(
      startIndex: Int
  )(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    for ((instr, i) <- instrList.zipWithIndex if i >= startIndex) {
      instr match {
        case Push(src, c1) =>
          if (indexIsInRange(i + 1, instrList.size)) {
            instrList(i + 1) match {
              case Pop(dest, c2) =>
                if (src.equals(dest) && c1.equals(c2)) {
                  instrList.remove(i, 2)
                  if (indexIsInRange(i - 1, instrList.size)) {
                    simplifyStackInstructions(i - 1)
                    return
                  }
                }
              case _ =>
            }
          }
        case Pop(dest, c1) =>
          if (indexIsInRange(i + 1, instrList.size)) {
            instrList(i + 1) match {
              case Push(src, c2) =>
                if (src.equals(dest) && c1.equals(c2)) {
                  instrList.remove(i, 2)
                  if (indexIsInRange(i - 1, instrList.size)) {
                    simplifyStackInstructions(i - 1)
                    return
                  }
                }
              case _ =>
            }
          }
        case _ =>
      }
    }
  }

  /*
     LDR r0 #x
     LDR r0 #y

     first instruction is redundant
   */
  def removeRedundantLoads(
      startIndex: Int
  )(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    for ((instr, i) <- instrList.zipWithIndex if i >= startIndex) {
      instr match {
        case Load(d1, _, c1) =>
          if (indexIsInRange(i + 1, instrList.size)) {
            instrList(i + 1) match {
              case Load(d2, _, c2) =>
                if (d1.equals(d2) && c1.equals(c2)) {
                  instrList.remove(i)
                  removeRedundantLoads(i)
                  return
                }
              case _ =>
            }
          }
        case LoadSByte(d1, _, c1) =>
          if (indexIsInRange(i + 1, instrList.size)) {
            instrList(i + 1) match {
              case LoadSByte(d2, _, c2) =>
                if (d1.equals(d2) && c1.equals(c2)) {
                  instrList.remove(i)
                  removeRedundantLoads(i)
                  return
                }
              case _ =>
            }
          }
        case _ =>
      }
    }
  }

  /*
   STR r0 #x
   STR r0 #y

   first instruction is redundant
   */
  def removeRedundantStores(
      startIndex: Int
  )(implicit instrList: mutable.Buffer[Instruction]): Unit = {
    for ((instr, i) <- instrList.zipWithIndex if i >= startIndex) {
      instr match {
        case Store(_, d1, c1) =>
          if (indexIsInRange(i + 1, instrList.size)) {
            instrList(i + 1) match {
              case Store(_, d2, c2) =>
                if (d1.equals(d2) && c1.equals(c2)) {
                  instrList.remove(i)
                  removeRedundantStores(i)
                  return
                }
              case _ =>
            }
          }
        case StoreByte(_, d1, c1) =>
          if (indexIsInRange(i + 1, instrList.size)) {
            instrList(i + 1) match {
              case StoreByte(_, d2, c2) =>
                if (d1.equals(d2) && c1.equals(c2)) {
                  instrList.remove(i)
                  removeRedundantStores(i)
                  return
                }
              case _ =>
            }
          }
        case _ =>
      }
    }
  }

  def indexIsInRange(i: Int, listSize: Int): Boolean = {
    0 <= i && i < listSize
  }
}
