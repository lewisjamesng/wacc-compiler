import LexerSemanticCheck.{AddNode, ArgListNode, ArrayElemNode, ArrayLiterNode, ArrayTypeNode, BeginNode, BoolBaseTypeNode, BoolLiterNode, BracketsNode, CallNode, CharBaseTypeNode, CharLiterNode, ChrNode, DivNode, EQNode, ExitNode, FreeNode, FstNode, GTENode, GTNode, IdentNode, IfNode, IntBaseTypeNode, IntLiterNode, LANDNode, LHSEqualsRHSNode, LORNode, LTENode, LTNode, LenNode, ModNode, MulNode, NEQNode, NegNode, NewIdentAssignNode, NewPairNode, NotNode, OrdNode, PairElemTypeBaseArrayNode, PairElemTypePairNode, PairLiterNode, PairTypeNode, ParamListNode, ParamNode, PrintNode, PrintlnNode, ReturnNode, SkipNode, SndNode, StatJoinNode, StrLiterNode, StringBaseTypeNode, SubNode, WhileNode}
import LexerSemanticCheck.ParserCombinator._
import parsley.{Failure, Result, Success}

import scala.language.implicitConversions

class ParserCombinatorTest extends ParsleyTest {

  it should "parse literals" in {
    literalParser.runParser(" 12345") shouldBe Success(IntLiterNode(12345))

    literalParser.runParser(" true ") shouldBe Success(BoolLiterNode(true))

    literalParser.runParser("[]") shouldBe Success(ArrayLiterNode(null, List()))

    literalParser.runParser("[1,2,3,4,5]") shouldBe Success(ArrayLiterNode(IntLiterNode(1), List(IntLiterNode(2),
      IntLiterNode(3), IntLiterNode(4), IntLiterNode(5))))

    literalParser.runParser("null") shouldBe Success(PairLiterNode())

    literalParser.runParser("\"abcdefg\"") shouldBe Success(StrLiterNode("abcdefg"))

    literalParser.runParser("'a'") shouldBe Success(CharLiterNode('a'))

    literalParser.runParser("(x)") shouldBe Success(BracketsNode(IdentNode("x")))

    literalParser.runParser("y") shouldBe Success(IdentNode("y"))
  }

  it should "parse expressions with variables" in {
    expressionParser.runParser("x + y") shouldBe Success(AddNode(IdentNode("x"), IdentNode("y")))

    expressionParser.runParser("chr y") shouldBe Success(ChrNode(IdentNode("y")))
  }

  it should "ignore whitespace and parse expressions" in {
    expressionParser.runParser("#1 add 1 \n\t (1 + 1)") shouldBe Success(BracketsNode(AddNode(IntLiterNode(1),
      IntLiterNode(1))))
  }

  it should "fail if brackets are mismatched or empty" in {
    expressionParser.runParser("((1)") shouldBe a [Failure]
    expressionParser.runParser("()") shouldBe a [Failure]
  }

  it should "parse binary operators" in {
    expressionParser.runParser("1 * 1") shouldBe Success(MulNode(IntLiterNode(1), IntLiterNode(1)))
    expressionParser.runParser("1 / 1") shouldBe Success(DivNode(IntLiterNode(1), IntLiterNode(1)))
    expressionParser.runParser("1 % 1") shouldBe Success(ModNode(IntLiterNode(1), IntLiterNode(1)))
    expressionParser.runParser("1 + 1") shouldBe Success(AddNode(IntLiterNode(1), IntLiterNode(1)))
    expressionParser.runParser("1 - 1") shouldBe Success(SubNode(IntLiterNode(1), IntLiterNode(1)))
    expressionParser.runParser("1 > 1") shouldBe Success(GTNode(IntLiterNode(1), IntLiterNode(1)))
    expressionParser.runParser("1 >= 1") shouldBe Success(GTENode(IntLiterNode(1), IntLiterNode(1)))
    expressionParser.runParser("1 < 1") shouldBe Success(LTNode(IntLiterNode(1), IntLiterNode(1)))
    expressionParser.runParser("1 <= 1") shouldBe Success(LTENode(IntLiterNode(1), IntLiterNode(1)))
    expressionParser.runParser("1 == 1") shouldBe Success(EQNode(IntLiterNode(1), IntLiterNode(1)))
    expressionParser.runParser("1 != 1") shouldBe Success(NEQNode(IntLiterNode(1), IntLiterNode(1)))
    expressionParser.runParser("1 && 1") shouldBe Success(LANDNode(IntLiterNode(1), IntLiterNode(1)))
    expressionParser.runParser("1 || 1") shouldBe Success(LORNode(IntLiterNode(1), IntLiterNode(1)))
  }

  it should "apply precedences when forming the AST" in {

    expressionParser.runParser("1 == 1 > 1") shouldBe (Success(EQNode(IntLiterNode(1), GTNode(IntLiterNode(1),
      IntLiterNode(1)))) : Result[EQNode])

    expressionParser.runParser("1 && 1 || 1") shouldBe Success(LORNode(LANDNode(IntLiterNode(1),
      IntLiterNode(1)), IntLiterNode(1)))

    expressionParser.runParser("1 / (1 + 1)") shouldBe Success(DivNode(IntLiterNode(1), BracketsNode(AddNode
    (IntLiterNode(1), IntLiterNode(1)))))

    expressionParser.runParser("(1 + 1 / 1 > 1 == 1 && 1 || 1)") shouldBe Success(BracketsNode(LORNode(LANDNode(EQNode(GTNode
    (AddNode(IntLiterNode(1), DivNode(IntLiterNode(1), IntLiterNode(1))), IntLiterNode(1)), IntLiterNode(1)),
      IntLiterNode(1)), IntLiterNode(1))))
  }

  it should "parse chars with escape characters" in {
    expressionParser.runParser("'\\\"'") shouldBe Success(CharLiterNode('"'))
    expressionParser.runParser("'\\\''") shouldBe Success(CharLiterNode('\''))
  }

  it should "parse unary operators" in {
    expressionParser.runParser("!true") shouldBe Success(NotNode(BoolLiterNode(true)))
    expressionParser.runParser("len x") shouldBe Success(LenNode(IdentNode("x")))
    expressionParser.runParser("ord 'a'") shouldBe Success(OrdNode(CharLiterNode('a')))
    expressionParser.runParser("chr 100") shouldBe Success(ChrNode(IntLiterNode(100)))
    expressionParser.runParser("-x") shouldBe Success(NegNode(IdentNode("x")))
  }

  it should "parse pair-type" in {
    pairTypeParser.runParser("pair(int, int)") shouldBe Success(PairTypeNode(PairElemTypeBaseArrayNode(IntBaseTypeNode()),
      PairElemTypeBaseArrayNode(IntBaseTypeNode())))
    pairTypeParser.runParser("pair(int, pair)") shouldBe Success(PairTypeNode(PairElemTypeBaseArrayNode
    (IntBaseTypeNode()), PairElemTypePairNode()))
  }

  it should "parse array-type" in {
    arrayTypeParser.runParser("int [][][]") shouldBe Success(ArrayTypeNode(ArrayTypeNode(ArrayTypeNode
    (IntBaseTypeNode()))))

    arrayTypeParser.runParser("pair(int, int)[][][]") shouldBe Success(ArrayTypeNode(ArrayTypeNode(ArrayTypeNode
    (PairTypeNode(PairElemTypeBaseArrayNode(IntBaseTypeNode()), PairElemTypeBaseArrayNode(IntBaseTypeNode()))))))
  }

  it should "parse base-type" in {
    baseTypeParser.runParser("int") shouldBe Success(IntBaseTypeNode())
    baseTypeParser.runParser("bool") shouldBe Success(BoolBaseTypeNode())
    baseTypeParser.runParser("string") shouldBe Success(StringBaseTypeNode())
    baseTypeParser.runParser("char") shouldBe Success(CharBaseTypeNode())
  }

  it should "parse type" in {
    newTypeParser.runParser("int []") shouldBe Success(ArrayTypeNode(IntBaseTypeNode()))

    newTypeParser.runParser("pair(int [], bool)") shouldBe Success(PairTypeNode(PairElemTypeBaseArrayNode
    (ArrayTypeNode(IntBaseTypeNode())), PairElemTypeBaseArrayNode(BoolBaseTypeNode())))

    newTypeParser.runParser("bool [][]") shouldBe Success(ArrayTypeNode(ArrayTypeNode(BoolBaseTypeNode())))

    newTypeParser.runParser("pair(pair, pair)") shouldBe Success(PairTypeNode(PairElemTypePairNode(),
      PairElemTypePairNode()))

    newTypeParser.runParser("pair(int, int)[]") shouldBe Success(ArrayTypeNode(PairTypeNode(PairElemTypeBaseArrayNode
    (IntBaseTypeNode()), PairElemTypeBaseArrayNode(IntBaseTypeNode()))))

    newTypeParser.runParser("pair(bool, string)") shouldBe Success(PairTypeNode(PairElemTypeBaseArrayNode
    (BoolBaseTypeNode()), PairElemTypeBaseArrayNode(StringBaseTypeNode())))

    //important test-case!!! handles nested array in pairs correctly
    newTypeParser.runParser("pair(pair(int, int)[], int)") shouldBe Success(PairTypeNode(PairElemTypeBaseArrayNode
    (ArrayTypeNode(PairTypeNode(PairElemTypeBaseArrayNode(IntBaseTypeNode()),
      PairElemTypeBaseArrayNode(IntBaseTypeNode())))), PairElemTypeBaseArrayNode(IntBaseTypeNode())))
  }

  it should "parse pair-elem" in {
    pairElemParser.runParser("fst (1 + 1)") shouldBe Success(FstNode(BracketsNode(AddNode(IntLiterNode(1),
      IntLiterNode(1)))))
    pairElemParser.runParser("snd (2 + 2)") shouldBe Success(SndNode(BracketsNode(AddNode(IntLiterNode(2),
      IntLiterNode(2)))))
  }

  it should "parse arg-list" in {
    argListParser.runParser("1,2,3,4,5") shouldBe Success(ArgListNode(List(IntLiterNode(1), IntLiterNode(2),
      IntLiterNode(3), IntLiterNode(4), IntLiterNode(5))))
    argListParser.runParser("1") shouldBe Success(ArgListNode(List(IntLiterNode(1))))
  }

  it should "parse assign-rhs" in {
    assignRhsParser.runParser("123") shouldBe Success(IntLiterNode(123))

    assignRhsParser.runParser("[1,2,3,4,5]") shouldBe Success(ArrayLiterNode(IntLiterNode(1), List(IntLiterNode(2),
      IntLiterNode(3), IntLiterNode(4), IntLiterNode(5))))

    assignRhsParser.runParser("newpair(first, second)") shouldBe Success(NewPairNode(IdentNode("first"),
      IdentNode("second")))

    assignRhsParser.runParser("call x(1, 2, 3, 4, 5)") shouldBe Success(CallNode(IdentNode("x"),
      ArgListNode(List(IntLiterNode(1), IntLiterNode(2), IntLiterNode(3), IntLiterNode(4),
        IntLiterNode(5)))))

    assignRhsParser.runParser("call x()") shouldBe Success(CallNode(IdentNode("x"),ArgListNode(List())))
  }

  it should "parse array elem" in {
    arrayElemParser.runParser("x[1][2][3]") shouldBe Success(ArrayElemNode(IdentNode("x"), List(IntLiterNode(1),
      IntLiterNode(2), IntLiterNode(3))))
  }

  it should "parse stat" in {
    statParser.runParser("skip") shouldBe Success(SkipNode())
    statParser.runParser("int x = 10") shouldBe Success(NewIdentAssignNode(IntBaseTypeNode(),IdentNode("x"),IntLiterNode(10)))
    statParser.runParser("x = 10") shouldBe Success(LHSEqualsRHSNode(IdentNode("x"),IntLiterNode(10)))
    statParser.runParser("free x") shouldBe Success(FreeNode(IdentNode("x")))
    statParser.runParser("return x") shouldBe Success(ReturnNode(IdentNode("x")))
    statParser.runParser("exit x") shouldBe Success(ExitNode(IdentNode("x")))
    statParser.runParser("print x") shouldBe Success(PrintNode(IdentNode("x")))
    statParser.runParser("println x") shouldBe Success(PrintlnNode(IdentNode("x")))

    statParser.runParser("if x then return 1 else return 1 fi") shouldBe Success(IfNode(IdentNode("x"),ReturnNode(IntLiterNode(1)),ReturnNode(IntLiterNode(1))))
    statParser.runParser("if x then skip ; return 1 else skip ; return 1 fi") shouldBe Success(IfNode(IdentNode("x"),StatJoinNode(List(SkipNode(), ReturnNode(IntLiterNode(1)))),StatJoinNode(List(SkipNode(), ReturnNode(IntLiterNode(1))))))
    statParser.runParser("while x do skip done") shouldBe Success(WhileNode(IdentNode("x"),SkipNode()))
    statParser.runParser("begin skip end") shouldBe Success(BeginNode(SkipNode()))
    statParser.runParser("skip ; skip") shouldBe Success(StatJoinNode(List(SkipNode(),SkipNode())))
  }

  it should "parse param" in {
    paramParser.runParser("int x") shouldBe Success(ParamNode(IntBaseTypeNode(), IdentNode("x")))
    paramParser.runParser("bool y") shouldBe Success(ParamNode(BoolBaseTypeNode(), IdentNode("y")))
  }

  it should "parse param-list" in {
    paramListParser.runParser("int x, bool y, char z") shouldBe Success(ParamListNode( List(ParamNode(IntBaseTypeNode(),
      IdentNode("x")),ParamNode(BoolBaseTypeNode(), IdentNode("y")), ParamNode(CharBaseTypeNode(), IdentNode("z")))))
  }
//
//  //  it should "parse func" in {
//      funcParser.runParser("\tint divide(int x1, int x2) is\n\t\t# We don't have int_64 in our language so we" +
//        " just ignore the overflow\n\t\tint ff = call f() ;\n\t\treturn x1 * ff / x2\n\tend") shouldBe Success(LexerSemanticCheck.FuncNode(LexerSemanticCheck.IntBaseTypeNode(),LexerSemanticCheck.IdentNode("divide"),LexerSemanticCheck.ParamListNode(LexerSemanticCheck.ParamNode(LexerSemanticCheck.IntBaseTypeNode(),LexerSemanticCheck.IdentNode("x1")),List(LexerSemanticCheck.ParamNode(LexerSemanticCheck.IntBaseTypeNode(),LexerSemanticCheck.IdentNode("x2")))),LexerSemanticCheck.StatJoinNode(List(LexerSemanticCheck.NewIdentAssignNode(LexerSemanticCheck.IntBaseTypeNode(),LexerSemanticCheck.IdentNode("ff"),LexerSemanticCheck.CallNode(LexerSemanticCheck.IdentNode("f"),null)), LexerSemanticCheck.ReturnNode(LexerSemanticCheck.DivNode(LexerSemanticCheck.MulNode(LexerSemanticCheck.IdentNode("x1"),LexerSemanticCheck.IdentNode("ff")),LexerSemanticCheck.IdentNode("x2"))))))))
//
//
//
//      funcParser.runParser("pair(int, pair) insert(pair(int, pair) " +
//        "root, int n) is\n    if root == null then\n      root = call createNewNode(n, null, null)\n    else\n     " +
//        " pair(pair, pair) p = snd root ;\n      int current = fst root ;\n      pair(int, pair) q = null ;\n     " +
//        " if n < current then\n      \tq = fst p ;\n        fst p = call insert(q, n)\n      else \n      \tq = " +
//        "snd p ;\n        snd p = call insert(q, n)\n      fi \n    fi ;\n    return root\n  end") shouldBe Success(Success(LexerSemanticCheck.FuncNode(LexerSemanticCheck.IntBaseTypeNode(),LexerSemanticCheck.IdentNode(divide),LexerSemanticCheck.ParamListNode(LexerSemanticCheck.ParamNode(LexerSemanticCheck.IntBaseTypeNode(),LexerSemanticCheck.IdentNode(x1)),List(LexerSemanticCheck.ParamNode(LexerSemanticCheck.IntBaseTypeNode(),LexerSemanticCheck.IdentNode(x2)))),LexerSemanticCheck.StatJoinNode(List(LexerSemanticCheck.NewIdentAssignNode(LexerSemanticCheck.IntBaseTypeNode(),LexerSemanticCheck.IdentNode(ff),LexerSemanticCheck.CallNode(LexerSemanticCheck.IdentNode(f),null)), LexerSemanticCheck.ReturnNode(LexerSemanticCheck.DivNode(LexerSemanticCheck.MulNode(LexerSemanticCheck.IdentNode("x1"),LexerSemanticCheck.IdentNode("ff")),LexerSemanticCheck.IdentNode("x2")))))))))
//
//      funcParser.runParser("  pair(int, pair) createNewNode(int value, pair(int, pair) left, pair(int, pair)" +
//        " right) is\n    pair(pair, pair) p = newpair(left, right) ;\n    pair(int, pair) q = newpair(value, p) ;\n" +
//        "    return q\n  end") shouldBe Success())
//
//      it should "parse programs" in {
//
//        programParser.runParser("begin skip end") shouldBe (Success())
//
//        programParser.runParser("# simple integer calculation\n\n# Output:\n# 72\n\n# Program:\n\nbegin\n  " +
//          "int x = 42 ;\n  int y = 30 ;\n  int z = x + y ;\n  println z\nend") shouldBe Success())
//
//        programParser.runParser("# evaluating greater-than\n\n# Output:\n# false\n# true\n\n# Program:\n\nbegin\n  " +
//          "int x = 2 ;\n  int y = 6 ;\n  int z = 4 ;\n  println x > y ;\n  println y > z\nend") shouldBe Success())
//      }

}