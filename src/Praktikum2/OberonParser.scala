package Praktikum2

object OberonParser extends App {
  import java.io.FileReader
  import Praktikum2._
  //  import Praktikum2.Token._
  import Praktikum2.OberonScanner._
  import Tree._

  val scanner = new OberonScanner(new FileReader("src/Examples/OberonExample2"))
  var current = next
  
  class Archive extends Iterable[Symbol] {
    val archive: List[Symbol] = List()

    class It extends Iterator[Symbol]{
//      def next = {
//        this.cou
//      }
    }
    def iterator = new It
  }

  //  println(mul)
  parser
  def parser: Unit = {
    val s = next
    if (s != None) {
      println(s.get.toLongString);
      parser
    }
  }

  def mul = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.mul)
      Tree(Token.mul, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def plus = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.plus)
      Tree(Token.plus, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def sub = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.sub)
      Tree(Token.sub, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def div = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.div)
      Tree(Token.div, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def _def = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token._def)
      Tree(Token._def, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def equ = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.equ)
      Tree(Token.equ, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def sharp = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.sharp)
      Tree(Token.sharp, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def smaller = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.smaller)
      Tree(Token.smaller, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def smallereq = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.smallereq)
      Tree(Token.smallereq, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def bigger = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.bigger)
      Tree(Token.bigger, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def biggereq = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.biggereq)
      Tree(Token.biggereq, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def comma = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.comma)
      Tree(Token.comma, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def semicolon = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.semicolon)
      Tree(Token.semicolon, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def bracketOn = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.bracketOn)
      Tree(Token.bracketOn, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def bracketOff = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.bracketOff)
      Tree(Token.bracketOff, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def camberedBracketOn = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.camberedBracketOn)
      Tree(Token.camberedBracketOn, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def camberedBracketOff = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.camberedBracketOff)
      Tree(Token.camberedBracketOff, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def colon = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.colon)
      Tree(Token.colon, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def ident = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.ident)
      Tree(Token.ident, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def string = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.string)
      Tree(Token.string, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def integer = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.integer)
      Tree(Token.integer, Tree(current))
    else {
      error(current)
      Nil
    }
  }
  def DOT = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == Token.DOT)
      Tree(Token.DOT, Tree(current))
    else {
      error(current)
      Nil
    }
  }

  //Selector         = {Õ.Õ ident | Õ[Õ Expression Õ]Õ}.
  def Selector = {
    if (Expression != Nil) {
      Tree('Selector, Expression)
    }
    if (DOT != Nil && inc && ident != Nil) {
      Tree('Selector, ident)
    } else {
      current
      error(current)
      Nil
    }
  }

  //Factor           = ident Selector | integer | string |
  //                    Read |
  //                   Õ(Õ Expression Õ)Õ.
  def Factor = {
    if (current.isEmpty)
      Nil
    //    else if (current.get.token == Token.ident && (s2.get.token == Selector || s2.get.token == Token.integer || s2.get.token == Token.string || READ))
    //      //      if (current.get.token)
    //      Node('ConstIdent, Node(current.get))
    else {
      //      error(s)
      Nil
    }
  }

  //Read             = READ [Prompt].
  def Read = Nil
  //Prompt           = string.
  def Prompt = Nil
  //Term             = Factor {(Õ*Õ | Õ/Õ) Factor}.
  def Term = Nil
  //SimpleExpression = [Õ-Õ] Term
  //                   {(Õ+Õ | Õ-Õ) Term}.
  def SimplExpression = Nil
  //Expression       = SimpleExpression
  //                   [(Õ=Õ | Õ#Õ | Õ<Õ |
  //                     Õ<=Õ | Õ>Õ | Õ>=Õ)
  //                    SimpleExpression].
  def Expression = Nil

  //IndexExpression  = integer | ConstIdent.
  def IndexExpression = {
    if (current.isEmpty)
      Nil
    //    else if (current.get.token == Token.integer || ConstIdent.value.getOrElse(false) == Token.integer)
    //      Node('IndexExpression, Node(s))
    else {
      error(current)
      Nil
    }
  }

  //ConstIdent       = ident.  
  def ConstIdent = {
    if (current.isEmpty)
      Nil
    //    else if (current.get.token == Token.ident)
    //      Node('ConstIdent, Node(s))
    else {
      error(current)
      Nil
    }
  }

  //IdentList = ident {Õ,Õ ident}.
  def IdentList = Nil
  //ArrayType = ÕARRAYÕ Õ[Õ IndexExpression Õ]Õ ÕOFÕ Type.
  def ArrayType = Nil
  //FieldList = [IdentList Õ:Õ Type].
  def FieldList = Nil
  //RecordType = ÕRECORDÕ FieldList {Õ;Õ FieldList} ÕENDÕ.
  def RecordType = Nil
  //Type = ident | ArrayType | RecordType.
  def Type = Nil
  //FPSection = [ÕVARÕ] IdentList Õ:Õ Type.
  def FPSection = Nil
  //FormalParameters = FPSection {Õ;Õ FPSection}.
  def FormalParameters = Nil
  //ProcedureHeading = ÕPROCEDUREÕ ident Õ(Õ [FormalParameters] Õ)Õ.
  def ProcedureHeading = Nil
  //ProcedureBody    = Declarations ÕBEGINÕ StatementSequence ÕENDÕ ident Õ.Õ.
  def ProcedureBody = Nil
  //ProcedureDeclaration = ProcedureHeading Õ;Õ ProcedureBody ident.
  def ProcedureDeclaration = Nil
  //Declarations     = [ÕCONSTÕ ident Õ=Õ Expression Õ;Õ
  //                            {ident Õ=Õ Expression Õ;Õ}]
  //                   [ÕTYPEÕ ident Õ=Õ Type Õ;Õ
  //                           {ident Õ=Õ Type Õ;Õ}]
  //                   [ÕVARÕ IdentList Õ:Õ Type Õ;Õ
  //                          {IdentList Õ:Õ Type Õ;Õ}]
  //                   {ProcedureDeclaration Õ;Õ}.
  def Declarations = Nil
  //Module           = ÕMODULEÕ ident Õ;Õ Declarations
  //                   ÕBEGINÕ StatementSequence
  //                   ÕENDÕ ident Õ.Õ.
  def Module = Nil
  //Assignment        = ident Selector Õ:=Õ Expression.
  def Assignment = Nil
  //ActualParameters  = Expression {Õ,Õ Expression}.
  def ActualParameters = Nil
  //ProcedureCall = ident Õ(Õ [ActualParameters] Õ)Õ.
  def ProcedureCall = Nil
  //IfStatement = ÕIFÕ Expression ÕTHENÕ StatementSequence
  //	{ÕELSIFÕ Expression ÕTHENÕ StatementSequence}
  //  	[ÕELSEÕ StatementSequence] ÕENDÕ.
  def IfStatement = Nil
  //WhileStatement = ÕWHILEÕ Expression ÕDOÕ StatementSequence ÕENDÕ.
  def WhileStatement = Nil
  //RepeatStatement = ÕREPEATÕ StatementSequence ÕUNTILÕ Expression.
  def RepeatStatement = Nil
  //Statement = [Assignment | ProcedureCall |
  //   IfStatement | ÕPRINTÕ Expression |
  //   WhileStatement | RepeatStatement].
  def Statement = Nil
  //StatementSequence = Statement {Õ;Õ Statement}.
  def StatementSequence = Nil

  def inc = {
    next
    true
  }

  def next: Option[Symbol] = {
    if (scanner.isEOF)
      None
    else {
      val symbol = scanner.next_token
      if (scanner.isEOF)
        None
      else if (symbol.token != Token.blank)
        Option(symbol)
      else
        next
    }
  }

  def urparser = {
    while (!scanner.isEOF) {
      val s: Symbol = scanner.next_token()
      if (!scanner.isEOF) {
        println(s.toLongString);
      }
    }
  }

  //  def error(wrongSymbol: Symbol,token: String*) {
  //    var s = "";
  //    for (i <- token){
  //      s += "<" + i + ">"
  //    }
  //    println(Symbol.linecolumn(wrongSymbol.line,wrongSymbol.column) + " Error:" + s + " expected")
  //  }
  def error(wrongSymbol: Option[Symbol]) {
    if (wrongSymbol.isEmpty)
      println(None)
    else
      println(Symbol.linecolumn(wrongSymbol.get.line, wrongSymbol.get.column) + " Error:" + wrongSymbol.get.token + " is missing")
  }
}

