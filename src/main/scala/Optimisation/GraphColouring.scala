package Optimisation

import CodeGen.Constants.r0
import CodeGen.Reg

object GraphColouring {
  var startReg: Int = 1

  def colourGraph(): Unit = {
    var currReg = startReg
    for (node <- InterferenceGraph.nodes) {
      /* Check if the variable can be used with the currently used registers */
      for(regNum <- startReg to currReg) {
        if (checkColour(node, regNum)) {
          node.reg = Reg(regNum)
        }
      }
      /* If not, use another register */
      if (node.reg == r0) {
        currReg += 1
        node.reg = Reg(currReg)
      }
    }

    /* TODO: Spill if reached maximum register capacity */
  }

  def checkColour(node: InterferenceGraphNode, regNum: Int): Boolean = {
    for (neighbour <- node.neighbours) {
      neighbour.reg match {
        case Reg(n) =>
          if (n == regNum) {
            return false
          }
        case _ => return false
      }
    }
    true
  }
}