package amyc.test

import amyc.parsing._
import org.junit.Test

class ParserTests extends TestSuite with amyc.MainHelpers {
  val pipeline = Lexer andThen Parser andThen treePrinterN("")

  val baseDir = "parser"

  val outputExt = "scala"

  @Test def testLL1 = {
    assert(Parser.program.isLL1)
  }

  @Test def testEmpty = shouldOutput("Empty")
  @Test def testLiterals = shouldOutput("Literals")
  @Test def testNuances = shouldOutput("testNuances")
  @Test def testAll = shouldOutput("TestAll")
  @Test def testCustom = shouldOutput("custom")
  @Test def testHandWritten = shouldOutput("handWrittenTests")
  @Test def testValDef2 = shouldOutput("valdef2test")

  @Test def testEmptyFile = shouldFail("EmptyFile")
  @Test def testIfPrecedence = shouldFail("IfPrecedence")
  @Test def testCommentClosedTwice = shouldFail("CommentClosedTwice")
  @Test def testValDef1 = shouldFail("valdef1test")
  @Test def testValDef3 = shouldFail("valdef3test")
  @Test def testValDef4 = shouldFail("valdef4test")
  @Test def testValDef5 = shouldFail("valdef5test")
  @Test def testOperandVal1 = shouldFail("testOperandVal1")
  @Test def testOperandVal2 = shouldFail("testOperandVal2")
  @Test def testOperandVal3 = shouldFail("testOperandVal3")
  @Test def testOperandVal4 = shouldFail("testOperandVal4")
  @Test def testOperandVal5 = shouldFail("testOperandVal5")
  @Test def testMatchBinOperand = shouldFail("testMatchBinOperand")
  @Test def testUnary1 = shouldFail("testUnary1")
  @Test def testUnary2 = shouldFail("testUnary2")
  @Test def testUnary3 = shouldFail("testUnary3")
  @Test def testUnary4 = shouldFail("testUnary4")
  @Test def testUnary5 = shouldFail("testUnary5")
  @Test def testStructure1 = shouldFail("testStructure1")
  @Test def testStructure2 = shouldFail("testStructure2")
  @Test def testStructure3 = shouldFail("testStructure3")
  @Test def testStructure4 = shouldFail("testStructure4")
  @Test def testStructure5 = shouldFail("testStructure5")
  @Test def testStructure6 = shouldFail("testStructure6")
  @Test def testStructure7 = shouldFail("testStructure7")
  @Test def testITE1 = shouldFail("testITE1")
  @Test def testITE2 = shouldFail("testITE2")
  @Test def testITE3 = shouldFail("testITE3")
  @Test def testSeqSemi1 = shouldFail("testSeqSemi1")
  @Test def testSeqSemi2 = shouldFail("testSeqSemi2")
  @Test def testPattern2= shouldFail("testPattern2")
  @Test def testPattern3 = shouldFail("testPattern3")
  @Test def zomtest1 = shouldFail("zomtest1")
  @Test def zomtest2 = shouldFail("zomtest2")
  @Test def zomtest3 = shouldFail("zomtest3")
  @Test def zomtest4 = shouldFail("zomtest4")
  @Test def zomtest6 = shouldFail("zomtest6")
  @Test def zomtest7 = shouldFail("zomtest7")
  @Test def zomtest8 = shouldFail("zomtest8")
  @Test def zomtest9 = shouldFail("zomtest9")
  @Test def zomtest10 = shouldFail("zomtest10")
  @Test def zomtest11 = shouldFail("zomtest11")
  @Test def zomtest12 = shouldFail("zomtest12")
}

