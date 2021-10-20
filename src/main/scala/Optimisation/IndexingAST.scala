package Optimisation

import LexerSemanticCheck._

object IndexingAST {

  var index: Int = 0

  def indexAST(node: ASTNode): Unit = {

    node match {
      case ProgramNode(funcList, stat) =>
        for (func <- funcList) {
          setIndexAndIncrement(func)
        }
        indexAST(stat)
      case funcNode: FuncNode =>
        funcNode.t.index = index
        for(param <- funcNode.p.ps) {
          param.index = index
        }
        indexAST(funcNode.stat)
      case skipNode: SkipNode =>
        setIndexAndIncrement(skipNode)
      case newIdentAssignNode: NewIdentAssignNode =>
        newIdentAssignNode.t.index = index
        newIdentAssignNode.rhs.index = index
        setIndexAndIncrement(newIdentAssignNode)
      case lhsEqualsRHSNode: LHSEqualsRHSNode =>
        lhsEqualsRHSNode.lhs.index = index
        lhsEqualsRHSNode.rhs.index = index
        setIndexAndIncrement(lhsEqualsRHSNode)
      case readNode: ReadNode =>
        readNode.lhs.index = index
        setIndexAndIncrement(readNode)
      case freeNode: FreeNode =>
        freeNode.e.index = index
        setIndexAndIncrement(freeNode)
      case returnNode: ReturnNode =>
        returnNode.e.index = index
        setIndexAndIncrement(returnNode)
      case exitNode: ExitNode =>
        exitNode.e.index = index
        setIndexAndIncrement(exitNode)
      case printNode: PrintNode =>
        printNode.e.index = index
        setIndexAndIncrement(printNode)
      case printlnNode: PrintlnNode =>
        printlnNode.e.index = index
        setIndexAndIncrement(printlnNode)
      case ifNode: IfNode =>
        ifNode.e.index = index
        indexAST(ifNode.s1)
        indexAST(ifNode.s2)
        setIndexAndIncrement(ifNode)
      case whileNode: WhileNode =>
        whileNode.e.index = index
        indexAST(whileNode.s)
        setIndexAndIncrement(whileNode)
      case statJoinNode: StatJoinNode =>
        for(s <- statJoinNode.statList) {
          indexAST(s)
        }
      case beginNode: BeginNode =>
        beginNode.s.index = index
        setIndexAndIncrement(beginNode)
      case unaryOperatorNode: UnaryOperatorNode =>
        indexAST(unaryOperatorNode.inner)
        unaryOperatorNode.index = index
      case binaryOperatorNode: BinaryOperatorNode =>
        indexAST(binaryOperatorNode.left)
        indexAST(binaryOperatorNode.right)
        binaryOperatorNode.index = index
      case _ =>
    }
  }

  def incrementIndex(): Unit = {
    index += 1
  }

  def setIndexAndIncrement(node: ASTNode): Unit = {
    node.index = index
    incrementIndex()
  }

}
