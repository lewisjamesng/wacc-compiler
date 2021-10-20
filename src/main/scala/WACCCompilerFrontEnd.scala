import CodeGen.{ARMAssemblyWriter, AssemblyWriter, CodeGenerator, x86AssemblyWriter}
import LexerSemanticCheck.{ParserCombinator, SemanticChecker, SymbolTable}
import Optimisation.{GraphColouring, IndexingAST, InterferenceGraph, InterferenceGraphNode}
import parsley.{Failure, Success}

import java.io.File

object WACCCompilerFrontEnd {
  def main(args: Array[String]): Unit = {
    assert(args.length == 3, "Correct usage: ./executable <filename> <ARM/x86> <graph-colouring>")

    /* Parser combinator */
    println("Testing file: " + args(0))
    val file = new File(args(0))
    val parseAndLexResult = ParserCombinator.programParser.parseFromFile(file)

    // Syntax checks
    parseAndLexResult match {
      case Success(result) =>
        /* Semantic checks */
        val semanticCheckerResult = new SemanticChecker(result)
        semanticCheckerResult.run()

        if (semanticCheckerResult.errorLog.thereAreErrors) {
          semanticCheckerResult.errorLog.printErrors()
          println("exit:200")
          System.exit(200)
        }

        if (args(2) == "optim") {
          /* Indexing Nodes */
          IndexingAST.indexAST(result)

          /* Optimisation: Graph Colouring */
          val symbolTable: Option[SymbolTable] = result.symbolTable
          val graph = InterferenceGraph
          graph.symbolTable = symbolTable
          graph.nodes = graph.getNodes
          result.interferenceRepr()
          graph.checkLiveRange()
          val graphColouring = GraphColouring
          graphColouring.colourGraph()
        }

        /* Code generation */
        val writer: AssemblyWriter = if (args(1) == "ARM") new ARMAssemblyWriter() else new x86AssemblyWriter()
        val cg = new CodeGenerator(writer)

        val optimise = args(2) == "optim"
        implicit val instructions = cg.generateCode(result, optimise)
        val f = new File(args(0))
        val fileName = f.getName.replaceFirst("[.][^.]+$", "")
        cg.writeToFile(s"$fileName.s")

        /* Exit with success */
        println("exit:0")
        System.exit(0)

      case Failure(_) =>
        println("#" + parseAndLexResult + "#")
        println("exit:100")
        System.exit(100)
    }
  }

}
