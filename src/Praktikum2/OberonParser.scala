package Praktikum2
import scala.annotation.tailrec

@tailrec
object OberonParser extends App {
  import java.io.FileReader
  import Praktikum2._
  //  import Praktikum2.Token._
  import Praktikum2.OberonScanner._
  import Tree._

  val scanner = new OberonScanner(new FileReader("src/Examples/OberonExample2"))
  var current = next

  //  println(mul)
  parser
  def parser: Unit = {
    val s = next
    if (s != None) {
      println(s.get.toLongString);
      parser
    }
  }
  // Terminals
  def mul = checkPrimitive(Token.mul)
  def plus = checkPrimitive(Token.plus)
  def sub = checkPrimitive(Token.sub)
  def div = checkPrimitive(Token.div)
  def _def = checkPrimitive(Token._def)
  def equ = checkPrimitive(Token.equ)
  def sharp = checkPrimitive(Token.sharp)
  def smaller = checkPrimitive(Token.smaller)
  def smallereq = checkPrimitive(Token.smallereq)
  def bigger = checkPrimitive(Token.bigger)
  def biggereq = checkPrimitive(Token.biggereq)
  def comma = checkPrimitive(Token.comma)
  def semicolon = checkPrimitive(Token.semicolon)
  def bracketOn = checkPrimitive(Token.bracketOn)
  def bracketOff = checkPrimitive(Token.bracketOff)
  def camberedBracketOn = checkPrimitive(Token.camberedBracketOn)
  def camberedBracketOff = checkPrimitive(Token.camberedBracketOff)
  def colon = checkPrimitive(Token.colon)
  def ident = checkPrimitive(Token.ident)
  def string = checkPrimitive(Token.string)
  def integer = checkPrimitive(Token.integer)
  def DOT = checkPrimitive(Token.DOT)
  def edgeBracketOn = checkPrimitive(Token.edgeBracketOn)
  def edgeBracketOff = checkPrimitive(Token.edgeBracketOff)
  def OF = checkPrimitive(Token.OF)
  def THEN = checkPrimitive(Token.THEN)
  def DO = checkPrimitive(Token.DO)
  def PRINT = checkPrimitive(Token.PRINT)
  def READ = checkPrimitive(Token.READ)
  def END = checkPrimitive(Token.END)
  def ELSE = checkPrimitive(Token.ELSE)
  def ELSEIF = checkPrimitive(Token.ELSEIF)
  def IF = checkPrimitive(Token.IF)
  def WHILE = checkPrimitive(Token.WHILE)
  def REPEAT = checkPrimitive(Token.REPEAT)
  def UNTIL = checkPrimitive(Token.UNTIL)
  def ARRAY = checkPrimitive(Token.ARRAY)
  def RECORD = checkPrimitive(Token.RECORD)
  def CONST = checkPrimitive(Token.CONST)
  def TYPE = checkPrimitive(Token.TYPE)
  def VAR = checkPrimitive(Token.VAR)
  def PROCEDURE = checkPrimitive(Token.PROCEDURE)
  def BEGIN = checkPrimitive(Token.BEGIN)
  def MODULE = checkPrimitive(Token.MODULE)
  
  // NonTermonals with Precondition:
  // current token is used

  //Selector         = {Õ.Õ ident | Õ[Õ Expression Õ]Õ}.
  def Selector: Tree[_] = {
    println("Selector")
    inc
    if (DOT != Nil) {
      inc
      val id = ident
      if (id != Nil)
        Tree('Selector, ident)
      else {
        error("Selector with ident after dot")
        Nil
      }
    }
    if (edgeBracketOn != Nil) {
      inc
      Tree('Selector, Expression)
    } else {
      error("Selector with . or {")
      Nil
    }
  }

  //Factor           = ident Selector | integer | string |
  //                    Read |
  //                   Õ(Õ Expression Õ)Õ.
  def Factor: Tree[_] = {
    println("Factor")
    inc
    if (ident != Nil) {
      inc
      Tree('Factor, Selector)
    } else if (integer != Nil) {
      Tree('Factor, integer)
    } else if (bracketOn != Nil) {
      inc
      val expr = Expression
      inc
      if (bracketOff == Nil) {
        error("Factor with (")
        Nil
      }
      Tree('Factor, expr)
    } else if (string != Nil) {
      Read
    } else
      error("Factor with ident or integer or string or Read or Õ(Õ")
    Nil
  }

  //Read             = READ [Prompt].
  def Read = {
    println("Read")
    inc
    val r = READ
    if (r != Nil) {
      inc
      Tree('Read, READ, Prompt)
    } else {
      error("Read with READ")
    }
  }

  //Prompt           = string.
  def Prompt = {
    println("Prompt")
    inc
    val s = string
    if (s != Nil) {
      Tree('string, s)
    } else {
      error("Prompt with string")
      Nil
    }
  }

  //Term             = Factor {(Õ*Õ | Õ/Õ) Factor}.
  def Term = {
    println("Term")
    inc
    val f = Factor
    if (f != Nil) {
      inc
      if (mul != Nil || div != Nil) {
        inc
        Tree('Term, f, OptionalTerm)
      } else
        Tree('Term, f)
    } else
      error("Term with Factor")
    Nil
  }

  def OptionalTerm: Tree[_] = {
    println("OptionalTerm")
    inc
    val f = Factor
    if (f != Nil) {
      inc
      if (mul != Nil || div != Nil) {
        inc
        Tree('Term, f, OptionalTerm)
      } else
        Tree('Term, f)
    } else
      // no error because its optional
      Nil
  }

  //SimpleExpression = [Õ-Õ] Term
  //                   {(Õ+Õ | Õ-Õ) Term}.
  def SimpleExpression = {
    println("SimpleExpression")
    inc
    val s = sub
    if (s != Nil) {
      inc
      Tree('SimpleExpression, s, Term)
    }
    Tree('SimpleExpression, s)
  }

  //Expression       = SimpleExpression
  //                   [(Õ=Õ | Õ#Õ | Õ<Õ |
  //                     Õ<=Õ | Õ>Õ | Õ>=Õ)
  //                    SimpleExpression].
  def Expression = {
    println("Expression")
    inc
    val s = SimpleExpression
    if (s != Nil) {
      inc
      if (equ != Nil || sharp != Nil || smaller != Nil || smallereq != Nil
        || bigger != Nil || biggereq != Nil) {
        inc
        Tree('Expression, s, OptionalExpression)
      }
      Tree('Expression, s)
    } else {
      error("Expression with SimpleExpression")
      Nil
    }
  }

  def OptionalExpression: Tree[_] = {
    println("OptionalExpression")
    inc
    val s = SimpleExpression
    if (s != Nil) {
      inc
      if (equ != Nil || sharp != Nil || smaller != Nil || smallereq != Nil
        || bigger != Nil || biggereq != Nil) {
        inc
        Tree('Expression, s, OptionalExpression)
      }
      Tree('Expression, s)
    } else {
      // no error because its optional
      Nil
    }
  }

  //IndexExpression  = integer | ConstIdent.
  def IndexExpression = {
    println("IndexExpression")
    inc
    val i = integer
    if (i != Nil)
      Tree('IndexExpression, i)
    else {
      Tree('IndexExpression, ConstIdent)
    }
  }

  //ConstIdent       = ident.  
  def ConstIdent = {
    println("ConstIdent")
    inc
    val id = ident
    if (id != Nil) {
      Tree('ConsIdent, ident)
    } else {
      error("ConstIdent with ident")
      Nil
    }
  }

  //IdentList = ident {Õ,Õ ident}.
  def IdentList = {
    println("IdentList")
    inc
    val id = ident
    if (id != Nil) {
      OptionalIdentList
    } else {
      error("IdentList with ident")
      Nil
    }
  }

  def OptionalIdentList: Tree[_] = {
    println("OptionalIdentList")
    inc
    val id = ident
    if (id != Nil) {
      Tree('IdentList, OptionalIdentList)
    } else
      Nil
  }

  //ArrayType = ÕARRAYÕ Õ[Õ IndexExpression Õ]Õ ÕOFÕ Type.
  def ArrayType: Tree[_] = {
    println("ArrayType")
    inc
    if (ARRAY != Nil) {
      inc
      if (camberedBracketOn != Nil) {
        inc
        val i = IndexExpression
        inc
        if (camberedBracketOff != Nil) {
          inc
          if (OF != Nil) {
            inc
            val t = Type
            if (t != Nil) {
              Tree('ArrayType, i, t)
            } else {
              error("Arraytype with Type after OF")
              Nil
            }
          } else {
            error("Arraytype with OF after }")
            Nil
          }
        } else {
          error("Arraytype with } after IndexExpression")
          Nil
        }
      } else {
        error("Arraytype with { after ARRAY")
        Nil
      }
    } else {
      error("Arraytype with ARRAY")
      Nil
    }
  }

  //FieldList = [IdentList Õ:Õ Type].
  def FieldList: Tree[_] = {
    println("FieldList")
    inc
    val idl = IdentList
    if (idl != Nil) {
      inc
      if (colon != Nil) {
        inc
        val t = Type
        if (t != Nil) {
          Tree('FieldList, idl, t)
        } else {
          error("Identlist with Type after :")
          Nil
        }
      } else {
        error("Idenlist with :")
        Nil
      }
    } else {
      error("Identlist")
      Nil
    }
  }

  //RecordType = ÕRECORDÕ FieldList {Õ;Õ FieldList} ÕENDÕ.
  def RecordType: Tree[_] = {
    println("RecordType")
    inc
    if (RECORD != Nil) {
      inc
      val f = FieldList
      if (f != Nil) {
        inc
        if (semicolon != Nil) {
          Tree('RecordType, OptionalFieldList)
        } else {
          Tree('RecordType)
        }
      } else {
        error("RecordType with FieldList")
        Nil
      }
    } else {
      error("RecordType with RECORD")
      Nil
    }
  }

  def OptionalFieldList: Tree[_] = {
    println("OptionalFieldList")
    inc
    if (semicolon != Nil) {
      inc
      val f = FieldList
      if (f != Nil) {
        Tree('Fieldlist, FieldList, OptionalFieldList)
      } else
        error("FieldList after ;")
      Nil
    } else
      Nil
  }

  //Type = ident | ArrayType | RecordType.
  def Type: Tree[_] = {
    println("Type")
    inc
    val id = ident
    val arr = ARRAY
    val rec = RECORD
    if (id != Nil) {
      Tree('Type, id)
    } else if (arr != Nil) {
      Tree('Type, ArrayType)
    } else if (RECORD != Nil) {
      Tree('Type, RecordType)
    } else {
      error("Type with ident or ARRAY or Record")
      Nil
    }
  }

  //FPSection = [ÕVARÕ] IdentList Õ:Õ Type.
  def FPSection = {
    println("FPSection")
    inc
    if (VAR != Nil) {
      inc
    }
    val idl = IdentList
    inc
    if (colon != Nil) {
      inc
      val t = Type
      if (t != Nil) {
        Tree('FPSection, Type)
      } else {
        error("FPSection with Type after :")
        Nil
      }
    } else {
      error("FPSection with : after VAR")
      Nil
    }
  }

  def OptionalFormalParameters: Tree[_] = {
    println("OptionalFPSection")
    inc
    if (semicolon != Nil) {
      inc
      val fps = FPSection
      if (fps != Nil) {
        Tree('FormalParameters, fps, OptionalFormalParameters)
      } else {
        Tree('FormalParameters, fps)
      }
    } else {
      // No error because its optional
      Nil
    }
  }

  //FormalParameters = FPSection {Õ;Õ FPSection}.
  def FormalParameters = {
    println("FormalParamters")
    inc
    val fps = FPSection
    if (fps != Nil) {
      Tree('FormalParameters, fps, OptionalFormalParameters)
    } else {
      error("FormalParameters with FPSection")
      Nil
    }
  }

  //ProcedureHeading = ÕPROCEDUREÕ ident Õ(Õ [FormalParameters] Õ)Õ.
  def ProcedureHeading = {
    println("ProcedureHeading")
    inc
    if (PROCEDURE != Nil) {
      inc
      val id = ident
      if (id != Nil) {
        inc
        if (bracketOff == Nil) {
          Tree('ProcedureHeading, id)
        } else {
          Tree('ProcedureHeading, id, FormalParameters)
        }
      } else {
        error("ProcedureHeading with id")
        Nil
      }
    } else {
      error("ProcedureHeading with PROCEDURE")
      Nil
    }
  }

  //ProcedureBody    = Declarations ÕBEGINÕ StatementSequence ÕENDÕ ident Õ.Õ.
  def ProcedureBody = {
    println("ProcedureBody")
    inc
    val decl = Declarations
    if (decl != Nil) {
      inc
      if (BEGIN != Nil) {
        inc
        val sts = StatementSequence
        if (StatementSequence != Nil) {
          inc
          if (END != Nil) {
            inc
            val id = ident
            if (id != Nil) {
              inc
              if (DOT != Nil) {
                Tree('ProcedureBody, sts, id)
              } else {
                error("ProcedureBody with")
                Nil
              }
            } else {
              error("ProcedureBody with Ident")
              Nil
            }
          } else {
            error("ProcedureBody with END")
            Nil
          }
        } else {
          error("ProcedureBody with Statementsequence")
          Nil
        }
      } else {
        error("ProcedureBody with BEGIN")
        Nil
      }
    } else {
      error("ProcedureBody with Declarations")
      Nil
    }
  }

  //ProcedureDeclaration = ProcedureHeading Õ;Õ ProcedureBody ident.
  def ProcedureDeclaration = {
    println("ProcedureDeclaration")
    inc
    val ph = ProcedureHeading
    if (ph != Nil){ 
      inc
      if (semicolon != Nil){
        inc
        val pb = ProcedureBody
        if (pb != Nil){
          inc
          if (ident != Nil){
            Tree('ProcedureDeclaration,ph,pb)
          }
          else{
            error("ProcedureDeclaration with ident")
            Nil
          }
        }else{
          error("ProcedureDeclaration with ProcedureBody") 
          Nil
        }
      }else{
        error("ProcedureDeclaration with ;")
        Nil
      }
    }else{
      error("ProcedureDeclaration with ProcedureHeading")
      Nil
    }
  }

  //Declarations     = [ÕCONSTÕ ident Õ=Õ Expression Õ;Õ
  //                            {ident Õ=Õ Expression Õ;Õ}]
  //                   [ÕTYPEÕ ident Õ=Õ Type Õ;Õ
  //                           {ident Õ=Õ Type Õ;Õ}]
  //                   [ÕVARÕ IdentList Õ:Õ Type Õ;Õ
  //                          {IdentList Õ:Õ Type Õ;Õ}]
  //                   {ProcedureDeclaration Õ;Õ}.
  def Declarations = {
    println("Declarations")
    Nil
  }

  //Module           = ÕMODULEÕ ident Õ;Õ Declarations
  //                   ÕBEGINÕ StatementSequence
  //                   ÕENDÕ ident Õ.Õ.
  def Module = {
    println("Module")
    Nil
  }

  //Assignment        = ident Selector Õ:=Õ Expression.
  def Assignment = {
    println("Assignment")
    Nil
  }

  //ActualParameters  = Expression {Õ,Õ Expression}.
  def ActualParameters = {
    println("ActualParameters")
    Nil
  }

  //ProcedureCall = ident Õ(Õ [ActualParameters] Õ)Õ.
  def ProcedureCall = {
    println("ProcedureCall")
    Nil
  }

  //IfStatement = ÕIFÕ Expression ÕTHENÕ StatementSequence
  //	{ÕELSIFÕ Expression ÕTHENÕ StatementSequence}
  //  	[ÕELSEÕ StatementSequence] ÕENDÕ.
  def IfStatement = {
    println("IfStatement")
    Nil
  }

  //WhileStatement = ÕWHILEÕ Expression ÕDOÕ StatementSequence ÕENDÕ.
  def WhileStatement = {
    println("WhileStatement")
    Nil
  }

  //RepeatStatement = ÕREPEATÕ StatementSequence ÕUNTILÕ Expression.
  def RepeatStatement = {
    println("RepeatStatement")
    Nil
  }

  //Statement = [Assignment | ProcedureCall |
  //   IfStatement | ÕPRINTÕ Expression |
  //   WhileStatement | RepeatStatement].
  def Statement = {
    println("Statement")
    Nil
  }

  //StatementSequence = Statement {Õ;Õ Statement}.
  def StatementSequence = {
    println("StatementSequence")
    Nil
  }

  /*
   *  helper methods
   */

  def inc = {
    current = next
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

  def checkPrimitive(token: String) = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == token)
      Tree(token, Tree(current))
    else {
      Nil
    }
  }

  def error(expectedToken: String) {
    if (current.isEmpty)
      println(None)
    else
      println(Symbol.linecolumn(current.get.line, current.get.column) + " Error:" + current.get.token + " not expected")
    println(" We expect:" + expectedToken)
    System.exit(-1)
  }
}

