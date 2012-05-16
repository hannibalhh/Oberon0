package Praktikum3

object MyOberon {

  def main(args: Array[String]): Unit = {
    JFlex.Main.generate(Array("src/Praktikum3/oberon.flex"))
  }

}