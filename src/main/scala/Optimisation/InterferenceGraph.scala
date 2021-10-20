package Optimisation

import LexerSemanticCheck.SymbolTable

import scala.collection.mutable

object InterferenceGraph {

  var symbolTable: Option[SymbolTable] = None
  var nodes: mutable.ListBuffer[InterferenceGraphNode] = getNodes

  def checkLiveRange(): Unit = {
    for (node <- nodes) {
      for (nextNode <- nodes) {
        if (node != nextNode && doesInterfere(node, nextNode)) {
          node.addEdge(nextNode)
        }
      }
    }
  }

  def doesInterfere(node1: InterferenceGraphNode, node2: InterferenceGraphNode): Boolean = {

    val (first, second) = if (node1.start > node2.start) (node1, node2) else (node2, node1)
    (first.start <= second.end) && (second.start < first.end)
  }

  def findIGNode(varName: String, index: Int): InterferenceGraphNode= {

    /* Search node with same name inside the graph */
    for (node <- nodes) {
      if(node.ident.equals(varName)) {
        return node
      }
    }
    /* If does not exist, add node to the graph */
    val newNode = InterferenceGraphNode(varName)
    newNode.start = index
    newNode.end = index
    nodes.addOne(newNode)

    newNode
  }

  /* Return a list buffer of variables stored in the symbol table
  * Return an empty list buffer if symbol table does not exist. */
  def getNodes: mutable.ListBuffer[InterferenceGraphNode] = {
    symbolTable match {
      case None => mutable.ListBuffer.empty
      case Some(sT) =>
        sT.nodes
    }
  }
}