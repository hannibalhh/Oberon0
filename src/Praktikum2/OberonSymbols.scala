package Praktikum2

object Token{
  val mul = "mul";
  val plus = "plus";
  val sub = "sub";
  val div = "div";
  val _def = "_def";
  val equ = "equ";
  val sharp = "sharp";
  val smaller = "smaller";
  val smallereq = "smallereq";
  val bigger = "bigger";
  val biggereq = "biggereq";
  val comma = "comma";
  val semicolon = "semicolon";
  val bracketOn = "bracketOn";
  val bracketOff = "bracketOff";
  val camberedBracketOn = "camberedBracketOn";
  val camberedBracketOff = "camberedBracketOff";
  val colon = "colon";
  val OF = "OF";
  val THEN = "THEN";
  val DO = "DO";
  val PRINT = "PRINT";
  val READ = "READ";
  val END = "END";
  val ELSE = "ELSE";
  val ELSEIF = "ELSEIF";
  val IF = "IF";
  val WHILE = "WHILE";
  val REPEAT = "REPEAT";
  val UNTIL = "UNTIL";
  val ARRAY = "ARRAY";
  val RECORD = "RECORD";
  val CONST = "CONST";
  val TYPE = "TYPE";
  val VAR = "VAR";
  val PROCEDURE = "PROCEDURE";
  val BEGIN = "BEGIN";
  val MODULE = "MODULE";
  val string = "string";
  val ident = "id";
  val integer = "integer";
  val DOT = "DOT";
  val blank = "blank";
  val EOF = "EOF";
}

  case class Symbol(token: String, line: Int, column: Int, value: Option[Object] = None) {
    def toLongString = {
      if (!value.isEmpty){
        Symbol.linecolumn(line, column) + " " + token + "(" + value.get + ")"
      }
      else{
        Symbol.linecolumn(line, column) + " " + token       
      }
    }
  }

  case object Symbol {
    def linecolumn(l: Int, p: Int) = {
      "(" + l + "," + p + ")	";
    }
    def symbolError(l: Int, p: Int, s: String) = {
      linecolumn(l, p) + "  Error: <" + s + "> Scanning was not successfull."
    }
  }

