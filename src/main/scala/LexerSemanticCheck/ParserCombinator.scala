package LexerSemanticCheck

import parsley.Parsley
import parsley.Parsley._
import parsley.character._
import parsley.combinator.{many, _}
import parsley.debug._
import parsley.expr._
import parsley.implicits.{Lift1, Lift2, Lift3, Lift4, Map2, charLift}
import parsley.token.{LanguageDef, Lexer, Parser, Predicate}

object ParserCombinator {

  private lazy val arrayElem: Parsley[ArrayElemNode] =
    ArrayElemNode
      .lift(ident, some(lexer.brackets(exprOrArrayLit)))
      .label("array element")

  implicit def implicitSymbol(s: String): Parsley[String] = lexer.symbol_(s)

  private lazy val bracketsExpr: Parsley[BracketsNode] =
    lexer.parens(exprOrArrayLit.map(BracketsNode)).label("parentheses")
  private lazy val expr: Parsley[ExprNode] =
    operators.label("expression")
  private lazy val exprOrArrayLit: Parsley[ExprNode] =
    (expr <\> arrayLiteral).label("expression or an array literal")
  private lazy val pairType: Parsley[PairTypeNode] =
    PairTypeNode
      .lift(
        lexer.keyword("pair") *> "(" *> pairElemType,
        "," *> pairElemType <* ")"
      )
      .label("pair-type")
  private lazy val arrayType: Parsley[TypeNode] = precedence[TypeNode](
    baseType <\> pairType,
    Ops(Postfix)("[]" #> ArrayTypeNode)
  )
  private lazy val pairElemType: Parsley[PairElemTypeNode] =
    (PairElemTypeBaseArrayNode.lift(arrayType <\> baseType) <\>
      lexer.keyword("pair") #> PairElemTypePairNode()).label("pair-elem-type")
  private lazy val newType: Parsley[TypeNode] =
    (arrayType <\> pairType <\> baseType).label("type")
  private lazy val pairElem: Parsley[ASTNode] =
    FstNode.lift(lexer.keyword("fst") *> exprOrArrayLit).label("Fst") <|>
      SndNode.lift(lexer.keyword("snd") *> exprOrArrayLit).label("Snd")
  private lazy val argList: Parsley[ArgListNode] =
    ArgListNode.lift(lexer.commaSep(exprOrArrayLit)).label("arg-list")
  private lazy val assignRhs: Parsley[ASTNode] =
    (arrayLiteral <\>
      NewPairNode.lift(
        lexer.keyword("newpair") *> "(" *> exprOrArrayLit,
        lexer.comma *> exprOrArrayLit <* ")"
      ) <\>
      pairElem <\>
      CallNode.lift(lexer.keyword("call") *> ident, lexer.parens(argList)) <\>
      exprOrArrayLit).label(
      "right-hand-side (array-lit, newpair, pair-elem, function-call or expression)"
    )
  private lazy val assignLhs: Parsley[ASTNode] =
    (arrayElem <\>
      pairElem <\>
      ident).label("left-hand-side (array-elem, pair-elem or valid identifier)")
  private lazy val statAtoms: Parsley[StatNode] = {
    (IfNode.lift(
      lexer.keyword("if") *> exprOrArrayLit,
      lexer.keyword("then") *> stat,
      lexer.keyword("else") *> stat <* lexer.keyword("fi")
    ) <|>
      WhileNode.lift(
        lexer.keyword("while") *> exprOrArrayLit,
        lexer.keyword("do") *> stat <* lexer.keyword("done")
      ) <|>
      BeginNode.lift(lexer.keyword("begin") *> stat <* lexer.keyword("end")) <|>
      NewIdentAssignNode.lift(newType, ident <* "=", assignRhs) <\>
      LHSEqualsRHSNode.lift(assignLhs <* "=", assignRhs) <\>
      ReadNode.lift(lexer.keyword("read") *> assignLhs) <|>
      lexer.keyword("free") *> exprOrArrayLit.map(FreeNode) <|>
      lexer.keyword("return") *> exprOrArrayLit.map(ReturnNode) <|>
      lexer.keyword("exit") *> exprOrArrayLit.map(ExitNode) <|>
      lexer.keyword("println") *> expr.map(PrintlnNode) <|>
      lexer.keyword("print") *> exprOrArrayLit.map(PrintNode) <|>
      "skip" #> SkipNode()).label("statement atom")
  }
  private lazy val stat =
    (statAtoms <* notFollowedBy(";") <\> statJoin).label("statement")
  private lazy val statJoin: Parsley[StatNode] =
    sepBy1(statAtoms, ";").map(StatJoinNode).label("stat-join")
  private lazy val param: Parsley[ParamNode] =
    ParamNode.lift(newType, ident).label("parameter")
  private lazy val paramList: Parsley[ParamListNode] =
    ParamListNode.lift(sepBy(param, ",")).label("parameter-list")
  private lazy val func: Parsley[FuncNode] =
    FuncNode
      .lift(
        newType,
        ident,
        lexer.parens(paramList),
        lexer.keyword("is") *> stat
          .guard(
            exitedPath,
            "Function body missing return or exit statement on a path"
          )
          .guard(
            noFunctionJunk,
            "Return statement is followed by garbage"
          ) <* lexer.keyword("end")
      )
      .label("function")
  private lazy val program: Parsley[ProgramNode] = ProgramNode
    .lift(
      lexer.keyword("begin")
        *> manyUntil(func, lookAhead(stat)),
      stat <* lexer.keyword("end")
    )
    .label("program")
  lazy val literalParser: Parsley[ExprNode] = lexer.whiteSpace *> literal <* eof
  lazy val pairTypeParser: Parsley[PairTypeNode] = lexer.whiteSpace *> pairType <* eof
  lazy val arrayTypeParser: Parsley[TypeNode] = lexer.whiteSpace *> arrayType <* eof
  lazy val baseTypeParser: Parsley[BaseTypeNode] = lexer.whiteSpace *> baseType <* eof
  lazy val newTypeParser: Parsley[TypeNode] = lexer.whiteSpace *> newType <* eof
  lazy val pairElemParser: Parsley[ASTNode] = lexer.whiteSpace *> pairElem <* eof
  lazy val argListParser: Parsley[ArgListNode] = lexer.whiteSpace *> argList <* eof
  lazy val assignRhsParser: Parsley[ASTNode] = lexer.whiteSpace *> assignRhs <* eof
  lazy val arrayElemParser: Parsley[ArrayElemNode] = lexer.whiteSpace *> arrayElem <* eof
  lazy val statParser: Parsley[StatNode] = lexer.whiteSpace *> stat <* eof
  lazy val paramParser: Parsley[ParamNode] = lexer.whiteSpace *> param <* eof
  lazy val paramListParser: Parsley[ParamListNode] = lexer.whiteSpace *> paramList <* eof
  lazy val expressionParser: Parsley[ExprNode] =
    lexer.whiteSpace *> exprOrArrayLit <* eof
  lazy val funcParser: Parsley[FuncNode] = lexer.whiteSpace *> func <* eof
  lazy val programParser: Parsley[ProgramNode] = lexer.whiteSpace *> program <* eof
  private val lexer = new Lexer(
    LanguageDef.plain.copy(
      commentLine = "#",
      space = Predicate(isWhitespace),
      keywords = Set(
        "len",
        "ord",
        "int",
        "bool",
        "string",
        "char",
        "chr",
        "begin",
        "end",
        "is",
        "skip",
        "read",
        "free",
        "return",
        "exit",
        "print",
        "println",
        "if",
        "then",
        "else",
        "fi",
        "while",
        "do",
        "done",
        "newpair",
        "call",
        "fst",
        "snd",
        "pair",
        "true",
        "false",
        "null"
      ),
      identStart = Parser(letter <|> char('_')),
      identLetter = Parser(alphaNum <|> char('_'))
    )
  )
  private val number =
    digit.foldLeft1[Long](0)((n, d) => n * 10 + d.asDigit) <* lexer.whiteSpace
  private val sign = '-' #> ((x: Long) => -x) <|> '+' #> identity[Long] _
  private val intLiteral =
    lexer.lexeme(
      ((sign <*> number)<|> number).collectMsg("integer overflow") {
        case x if x == x.toInt => IntLiterNode(x.toInt)
      }
    )

  private val boolLiteral = {
    (lexer.keyword("true") #> true <|>
      lexer.keyword("false") #> false).map(BoolLiterNode).label("boolean")
  }

  private val escapeChar = choice(
    '0' #> '\u0000',
    'b' #> '\b',
    't' #> '\t',
    'n' #> '\n',
    'f' #> '\f',
    'r' #> '\r',
    '"' #> '\"',
    '\'',
    '\\'
  ).label("escape char")
  private val character =
    noneOf('\\', '\'', '"') <\> ("\\" *> escapeChar).label("character")
  private val charLiteral =
    between(char('\''), "'", character.map(CharLiterNode))
      .label("character literal")
  private val strLiteral =
    between(char('"'), "\"", many(character).map(x => StrLiterNode(x.mkString)))
      .label("string literal")
  private val pairLiteral =
    (lexer.keyword("null") #> PairLiterNode()).label("pair literal")

  private val arrayLiteral: Parsley[ArrayLiterNode] =
    lexer.keyword("[]") #> ArrayLiterNode(null, List()) <|>
      lexer
        .brackets(lexer.commaSep(exprOrArrayLit))
        .map(list => ArrayLiterNode(list.head, list.tail))
        .label("array literal")

  private val ident = lexer.identifier.map(IdentNode).label("valid identifier")
  private val literal: Parsley[ExprNode] =
    ( intLiteral <\>
      boolLiteral <\>
      charLiteral <\>
      strLiteral <\>
      bracketsExpr <\>
      pairLiteral <\>
      arrayElem <\>
      ident)
  private val operators = precedence[ExprNode](
    literal,
    Ops(Prefix)(
      "!" #> NotNode,
      lexer.lexeme(attempt('-' <* notFollowedBy(digit))) #> NegNode,
      lexer.keyword("len") #> LenNode,
      lexer.keyword("ord") #> OrdNode,
      lexer.keyword("chr") #> ChrNode
    ),
    Ops(InfixL)("*" #> MulNode, "/" #> DivNode, "%" #> ModNode),
    Ops(InfixL)("+" #> AddNode, "-" #> SubNode),
    Ops(InfixL)(">=" #> GTENode, "<=" #> LTENode),
    Ops(InfixL)(">" #> GTNode, "<" #> LTNode),
    Ops(InfixL)("==" #> EQNode, "!=" #> NEQNode),
    Ops(InfixL)("&&" #> LANDNode),
    Ops(InfixL)("||" #> LORNode)
  ).label("binary or unary operator")
  private val baseType: Parsley[BaseTypeNode] =
    (lexer.keyword("int") #> IntBaseTypeNode() <|>
      lexer.keyword("bool") #> BoolBaseTypeNode() <|>
      lexer.keyword("char") #> CharBaseTypeNode() <|>
      lexer.keyword("string") #> StringBaseTypeNode()).label("base-type")

  private def exitedPath(statNode: StatNode): Boolean = {
    statNode match {
      case IfNode(_, s1, s2) => exitedPath(s1) && exitedPath(s2)
      case WhileNode(_, s)   => exitedPath(s)
      case StatJoinNode(statList) =>
        statList.foldLeft(false)((a, b) => a || exitedPath(b))
      case BeginNode(s)                => exitedPath(s)
      case _: ReturnNode | _: ExitNode => true
      case _                           => false
    }
  }

  private def noFunctionJunk(statNode: StatNode): Boolean = {
    statNode match {
      case IfNode(_, s1, s2) => noFunctionJunk(s1) && noFunctionJunk(s2)
      case WhileNode(_, s)   => noFunctionJunk(s)
      case StatJoinNode(statList) =>
        for (i <- statList.indices) {
          statList(i) match {
            case _: ReturnNode | _: ExitNode => return i == statList.length - 1
            case _                           => ()
          }
        }
        true
      case BeginNode(s)                => noFunctionJunk(s)
      case _: ReturnNode | _: ExitNode => true
      case _                           => false
    }
  }
}
