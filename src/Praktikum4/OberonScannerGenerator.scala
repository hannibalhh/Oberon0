package Praktikum4

object MyOberon {

  def main(args: Array[String]): Unit = {
    JFlex.Main.generate(Array("src/Praktikum4/oberon.flex"))
  }

}