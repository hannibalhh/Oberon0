package Praktikum4
import cip.base.CodeGen
import cip.instructions.ContInstruction

object OberonCodeGenerator {
  import Praktikum4.Tree._
  def run(abstractSyntaxTree: Tree[_]) = {
    import cip.base.CodeGen
    val codeGen = new CodeGen
    codeGen.start
    if (OberonDebug.abstractSyntaxTree)
      println(abstractSyntaxTree)
    abstractSyntaxTree.compile()
    if (OberonDebug.symbolTable)
      println(Memory.SymbolTables)
    codeGen.close
    abstractSyntaxTree
  }

}

// StandaloneCG.main(Array("assemblerCode", "0"))