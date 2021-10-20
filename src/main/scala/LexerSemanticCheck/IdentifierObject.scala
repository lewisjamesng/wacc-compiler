package LexerSemanticCheck

abstract class IDENTIFIER() {
  def getType: String
}

class TYPE(t: String) extends IDENTIFIER {
  override def getType: String = t
}

class VARIABLE(t: TYPE) extends IDENTIFIER {
  override def getType: String = t.getType
}

class PARAM(t: String) extends IDENTIFIER {
  override def getType: String = t
}

class FUNCTION(returnType: TYPE) extends IDENTIFIER {
  override def getType: String = returnType.getType

  var formals: List[PARAM] = List()
  var symbolTable: Option[SymbolTable] = None

  def paramAppend(p: PARAM): Unit = {
    formals :+= p
  }

  def setSymbolTable(s: SymbolTable): Unit = {
    symbolTable = Option(s)
  }
}


