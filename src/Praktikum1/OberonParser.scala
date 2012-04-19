package Praktikum1

object OberonParser extends App {
  import java.io.FileReader
  import Praktikum1.SymbolUtil._
  import java_cup.runtime.Symbol
  import Praktikum1.OberonScanner._
  import OberonSymbols._

  val scanner = new OberonScanner(new FileReader("src/Examples/OberonExample2"))

  parser

  def parser: Unit = {
    val s = next
    if (s != None) {
      println(symbolToString(s.get));
      parser
    }
  }

  def next: Option[Symbol] = {
    if (scanner.isEOF)
      None
    else {
      val token = scanner.next_token
      if (scanner.isEOF)
        None
      else if (tokenType(token) != blank)
        Option(token)
      else
        next
    }
  }

  def urparser = {
    while (!scanner.isEOF) {
      val s: Symbol = scanner.next_token
      if (!scanner.isEOF) {
        println(symbolToString(s));
      }
    }
  }

  //  def findSymbol = {
  //    while (!scanner.isEOF) {
  //      val s: Symbol = scanner.next_token
  //      if (!scanner.isEOF) {
  //        if (tokenType(s) == semicolon) {
  //          println("Semikolon")
  //        }
  //      }
  //    }
  //  }
  //  var labcnt = 0;
  //  var infile = "";
  //
  //  def compile(str: String) { println(str); }
  //  def outStr(str: String) { print(str + " "); }
  //  def outInt(i: Int) { print(i + " "); }
  //  def outOp(op: String) { print(op); }
  //  def error(str: String) { println("Error: " + str); }
}

