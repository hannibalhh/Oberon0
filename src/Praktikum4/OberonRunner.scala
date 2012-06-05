package Praktikum4
import cip.standaloneCodeGen.StandaloneCG

object OberonRunner extends App{
  run
  def run = StandaloneCG.main(Array("assemblerCode", debug))
  def runCorrect = StandaloneCG.main(Array("assemblerCorrectCode", debug))
  private def debug = if (OberonDebug.interpreter) "1" else "0" 
}