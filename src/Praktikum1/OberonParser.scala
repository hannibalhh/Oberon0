package Praktikum1

object OberonParser {
  import java.io.FileReader
  import Praktikum1.SymbolUtil._
  import java_cup.runtime.Symbol
  import Praktikum1.OberonScanner._
  import OberonSymbols._

  def main(args: Array[String]) = {
    urparser
  }

  val scanner = new OberonScanner(new FileReader("src/Praktikum1/OberonExample2"))

  def urparser = {
    while (!scanner.isEOF) {
      val s: Symbol = scanner.next_token()
      if (!scanner.isEOF) {
        println(symbolToString(s));
      }
    }
  }
  
  def findSymbol = {
    while (!scanner.isEOF) {
      val s: Symbol = scanner.next_token
      if (!scanner.isEOF) {
        if (tokenType(s) == semicolon) {
          println("Semikolon")
        }
      }
    }
  }

  var nextsymbol = scanner.next_token();
  var labcnt = 0;
  var infile = "";

  def compile(str: String) { println(str); }
  def outStr(str: String) { print(str + " "); }
  def outInt(i: Int) { print(i + " "); }
  def outOp(op: String) { print(op); }
  def error(str: String) { println("Error: " + str); }
}

