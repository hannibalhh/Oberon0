package Praktikum1

object MyOberon {

  def main(args: Array[String]): Unit = {
    JFlex.Main.generate(Array("src/Praktikum1/oberon.flex"))
  }

}