package LexerSemanticCheck

class SemanticChecker(var node: ASTNode) {
  private val _errorLog = SemanticErrorLog
  private val rootSymbolTable = new SymbolTable(None, errorLog)

  def errorLog: SemanticErrorLog.type = _errorLog

  def resetSemanticChecker(node: ASTNode): Unit = {
    _errorLog.empty()
    this.node = node
  }

  def run(): Unit = rootSymbolTable.semanticCheck(node)
}
