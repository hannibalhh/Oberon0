package Praktikum1

object OberonSymbols{
  private val s = 255;
  val mul = 1 + s;
  val plus = 2 + s;
  val sub = 3 + s;
  val div = 4 + s;
  val _def = 5 + s;
  val equ = 6 + s;
  val sharp = 7 + s;
  val smaller = 8 + s;
  val smallereq = 9 + s;
  val bigger = 10 + s;
  val biggereq = 11 + s;
  val comma = 12 + s;
  val semicolon = 13 + s;
  val bracketOn = 14 + s;
  val bracketOff = 15 + s;
  val camberedBracketOn = 16 + s;
  val camberedBracketOff = 17 + s;
  val colon = 18 + s;
  val OF = 19 + s;
  val THEN = 20 + s;
  val DO = 21 + s;
  val PRINT = 22 + s;
  val READ = 23 + s;
  val END = 24 + s;
  val ELSE = 25 + s;
  val ELSEIF = 26 + s;
  val IF = 27 + s;
  val WHILE = 28 + s;
  val REPEAT = 29 + s;
  val UNTIL = 30 + s;
  val ARRAY = 31 + s;
  val RECORD = 32 + s;
  val CONST = 33 + s;
  val TYPE = 34 + s;
  val VAR = 35 + s;
  val PROCEDURE = 36 + s;
  val BEGIN = 37 + s;
  val MODULE = 38 + s;
  val string = 39 + s;
  val id = 40 + s;
  val digit = 41 + s;
  val DOT = 42 + s;
  val blank = 43 + s;
}

object SymbolUtil {
  import java_cup.runtime.Symbol
  import Praktikum1.OberonScanner._



  def tokenTypeName(value: Int): String = {
    for (elem <- Class.forName("Praktikum1.OberonSymbols").getDeclaredMethods()) {
      if (elem.invoke(()) == value)
        return elem.getName();
    }
    ""
  }
  
  def tokenType(s: Symbol) = Integer.parseInt(s.sym.toString())

  def symbolToString(s: Symbol) = {
    val linecolumn = linecolumnToString(s.left, s.right)
    val value = if (s.value != null) ": " + s.value.toString else ""
    linecolumn + "  " + tokenTypeName(tokenType(s)) + value;
  }

  def linecolumnToString(l: Int, p: Int) = {
    "(" + l + "," + p + ")	";
  }

  def symbolError(l: Int, p: Int, s: String) = {
    linecolumnToString(l, p) + "  Error: <" + s + "> Scanning was not successfull."
  }
}

//object OberonSymbols extends Enumeration {
//  private type OberonSymbols = Value
//  val mul,
//      plus,
//      sub,
//      div,
//      _def,
//      equ,
//      sharp,
//      smaller,
//      smallereq,
//      bigger,
//      biggereq,
//      comma,
//      semicolon,
//      bracketOn,
//      bracketOff,
//      camberedBracketOn,
//      camberedBracketOff,
//      colon,
//      OF,
//      THEN,
//      DO,
//      PRINT,
//      READ,
//      END,
//      ELSE,
//      ELSEIF,
//      IF,
//      WHILE,
//      REPEAT,
//      UNTIL,
//      ARRAY,
//      RECORD,
//      CONST,
//      TYPE,
//      VAR,
//      PROCEDURE,
//      BEGIN,
//      MODULE,
//      string,
//      id,
//      digit,
//      DOT,
//		blank = Value
//}