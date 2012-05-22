package Praktikum3
import cip.base.CodeGen
import cip.instructions.ContInstruction

object Test extends App {

  val codeGen = new CodeGen
  codeGen.start
  val abstractSyntaxTree = OberonParser.parser
  abstractSyntaxTree.compile()
  codeGen.close

}