package Optimisation

import scala.collection.mutable
import CodeGen.{Reg, Register}
import CodeGen.Constants.r0

/* Interference Graph Node */
case class InterferenceGraphNode(ident: String, var reg: Register = r0) {

  var start: Int = 0
  var end: Int = 0
  var neighbours: mutable.ListBuffer[InterferenceGraphNode] = mutable.ListBuffer.empty

  def addEdge(node: InterferenceGraphNode): Unit = {
    if(node != this) {
      neighbours.addOne(node)
    }
  }

  def addAll(node: InterferenceGraphNode): Unit = {
    addEdge(node)

    for(n <- neighbours) {
      n.addEdge(node)
    }
  }

}
