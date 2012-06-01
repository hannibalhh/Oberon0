package Praktikum4
import cip.standaloneCodeGen.StandaloneCG

object OberonRunner extends App{
  runCorrect
  def run = StandaloneCG.main(Array("assemblerCode", "0"))
  def runCorrect = StandaloneCG.main(Array("assemblerCorrectCode", "0"))
}