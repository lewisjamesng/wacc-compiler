import LexerSemanticCheck.{ArrayLiterNode, BoolBaseTypeNode, BoolLiterNode, BracketsNode, CharLiterNode, ExitNode, IdentNode, IntBaseTypeNode, IntLiterNode, LHSEqualsRHSNode, NewIdentAssignNode, PairLiterNode, ProgramNode, SemanticChecker, StatJoinNode, StrLiterNode}
import org.scalatest.Assertions
import org.scalatest.flatspec.AnyFlatSpec

class SemanticCheckerTest extends AnyFlatSpec with Assertions {
  var tester = new SemanticChecker(null)
  "The semantic checker" should "check literals" in {
    tester.resetSemanticChecker(IntLiterNode(12345))
    tester.run()
    assert(!tester.errorLog.thereAreErrors)

    tester.resetSemanticChecker(BoolLiterNode(true))
    tester.run()
    assert(!tester.errorLog.thereAreErrors)

    tester.resetSemanticChecker(ArrayLiterNode(null, List()))
    tester.run()
    assert(!tester.errorLog.thereAreErrors)

    tester.resetSemanticChecker(
      ArrayLiterNode(
        IntLiterNode(1),
        List(IntLiterNode(2), IntLiterNode(3), IntLiterNode(4), IntLiterNode(5))
      )
    )
    tester.run()
    assert(!tester.errorLog.thereAreErrors)

    tester.resetSemanticChecker(PairLiterNode())
    tester.run()
    assert(!tester.errorLog.thereAreErrors)

    tester.resetSemanticChecker(StrLiterNode("abcdefg"))
    tester.run()
    assert(!tester.errorLog.thereAreErrors)

    tester.resetSemanticChecker(CharLiterNode('a'))
    tester.run()
    assert(!tester.errorLog.thereAreErrors)

    tester.resetSemanticChecker(BracketsNode(IntLiterNode(1)))
    tester.run()
    assert(!tester.errorLog.thereAreErrors)

    tester.resetSemanticChecker(IdentNode("y"))
    tester.run()
    assert(tester.errorLog.thereAreErrors)
  }

  it should "pass simple program 1" in {
    tester.resetSemanticChecker(ProgramNode(List(), StatJoinNode(List(NewIdentAssignNode(IntBaseTypeNode(), IdentNode("x"), IntLiterNode(10)), LHSEqualsRHSNode(IdentNode("x"), IntLiterNode(20)), ExitNode(IdentNode("x"))))))
    tester.run()
    assert(!tester.errorLog.thereAreErrors)
  }

  it should "fail simple program 1 when changed" in {
    tester.resetSemanticChecker(ProgramNode(List(), StatJoinNode(List(NewIdentAssignNode(BoolBaseTypeNode(), IdentNode("x"), IntLiterNode(10)), LHSEqualsRHSNode(IdentNode("x"), IntLiterNode(20)), ExitNode(IdentNode("x"))))))
    tester.run()
    tester.errorLog.printErrors()
    assert(tester.errorLog.thereAreErrors)
  }
}
