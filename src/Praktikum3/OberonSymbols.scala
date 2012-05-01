package Praktikum3

object Token{
  val mul = "*"
  val plus = "+"
  val sub = "-"
  val div = "/"
  val _def = ":="
  val equ = "="
  val sharp = "#"
  val smaller = "<"
  val smallereq = "<="
  val bigger = ">"
  val biggereq = ">="
  val comma = ","
  val semicolon = ";"
  val bracketOn = "("
  val bracketOff = ")"
  val camberedBracketOn = "{"
  val camberedBracketOff = "}"
  val colon = ":"
  val OF = "OF"
  val THEN = "THEN"
  val DO = "DO"
  val PRINT = "PRINT"
  val READ = "READ"
  val END = "END"
  val ELSE = "ELSE"
  val ELSEIF = "ELSEIF"
  val IF = "IF"
  val WHILE = "WHILE"
  val REPEAT = "REPEAT"
  val UNTIL = "UNTIL"
  val ARRAY = "ARRAY"
  val RECORD = "RECORD"
  val CONST = "CONST"
  val TYPE = "TYPE"
  val VAR = "VAR"
  val PROCEDURE = "PROCEDURE"
  val BEGIN = "BEGIN"
  val MODULE = "MODULE"
  val string = "string"
  val ident = "ident"
  val integer = "integer"
  val DOT = "DOT"
  val blank = "blank"
  val EOF = "EOF"
  val edgeBracketOn = "["
  val edgeBracketOff = "]"
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
      "(" + l + "," + p + ")	"
    }
    def symbolError(l: Int, p: Int, s: String) = {
      linecolumn(l, p) + "  Error: <" + s + "> Scanning was not successfull."
    }
  }

