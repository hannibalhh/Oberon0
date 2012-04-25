package Praktikum2
import scala.annotation.tailrec

@tailrec
object OberonParser extends App {
  import Praktikum2._
  import Praktikum2.OberonScanner._
  import Tree._

  val scanner = oberonScanner("src/Examples/NT/Module")
  var current = next
  def parser = {
    val m = Module
    if (current != None) {
      error("No Token")
      Nil
    }
    m
  }

  println(test(Module))

  /*
   *  terminals
   */

  // operators
  def mul = checkPrimitive(Token.mul)
  def plus = checkPrimitiveTree(Token.plus)
  def sub = checkPrimitiveTree(Token.sub)
  def div = checkPrimitive(Token.div)
  def _def = checkPrimitiveTree(Token._def)
  def equ = checkPrimitiveTree(Token.equ)
  def sharp = checkPrimitiveTree(Token.sharp)
  def smaller = checkPrimitiveTree(Token.smaller)
  def smallereq = checkPrimitiveTree(Token.smallereq)
  def bigger = checkPrimitiveTree(Token.bigger)
  def biggereq = checkPrimitiveTree(Token.biggereq)
  // terminals with values
  def ident = checkPrimitiveTree(Token.ident)
  def string = checkPrimitiveTree(Token.string)
  def integer = checkPrimitiveTree(Token.integer)
  // only syntax terminals (not allowed in AbstractSyntaxTree)
  def comma = checkPrimitive(Token.comma)
  def semicolon = checkPrimitive(Token.semicolon)
  def bracketOn = checkPrimitive(Token.bracketOn)
  def bracketOff = checkPrimitive(Token.bracketOff)
  def camberedBracketOn = checkPrimitive(Token.camberedBracketOn)
  def camberedBracketOff = checkPrimitive(Token.camberedBracketOff)
  def colon = checkPrimitive(Token.colon)
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

  /*
   *  NonTerminals 
   *  with Precondition: current token is NOT used
   */

  //Selector         = {Õ.Õ ident | Õ[Õ Expression Õ]Õ}.
  def Selector: Tree[_] = {
    println("Selector")
    if (DOT) {
      inc
      val id = ident
      if (id != Nil) {
        inc
        Tree('Selector, ident, Selector)
      } else {
        error("Selector with ident after dot")
        Nil
      }
    } else if (edgeBracketOn) {
      inc
      Tree('Selector, Expression, Selector)
    } else {
      // no error because its optional
      Nil
    }
  }

  //Factor           = ident Selector | integer | string |
  //                    Read |
  //                   Õ(Õ Expression Õ)Õ.
  // tested ident Selector
  def Factor: Tree[_] = {
    println("Factor")
    val id = ident
    val int = integer
    val s = string
    if (id != Nil) {
      inc
      Tree('Factor, id, Selector)
    } else if (int != Nil) {
      inc
      Tree('Factor, int)
    } else if (s != Nil) {
      inc
      Tree('Factor, s)
    } else if (bracketOn) {
      inc
      val expr = Expression
      if (bracketOff) {
        inc
        Tree('Factor, expr)
      } else {
        error("Factor ends with )")
        Nil
      }
    } else if (READ) {
      // no inc, because we look forward
      Tree('Factor, Read)
    } else {
      error("Factor with ident or integer or string or Read or Õ(Õ")
      Nil
    }
  }

  //Read             = READ [Prompt]. // tested
  def Read = {
    println("Read")
    val r = READ
    if (r) {
      inc
      Tree('Read, Prompt)
    } else {
      error("Read with READ")
      Nil
    }
  }

  //Prompt           = string. // tested
  def Prompt = {
    println("Prompt")
    val s = string
    if (s != Nil) {
      inc
      Tree('string, s)
    } else {
      error("Prompt with string")
      Nil
    }
  }

  //Term             = Factor {(Õ*Õ | Õ/Õ) Factor}.
  // tested int * int
  def Term = {
    println("Term")
    val f = Factor
    if (f != Nil) {
      Tree('Term, f, OptionalTerm)
    } else {
      error("Term with Factor")
      Nil
    }
  }

  def OptionalTerm: Tree[_] = {
    println("OptionalTerm")
    val m = mul
    val d = div
    if (m || d) {
      inc
      val f = Factor
      if (f != Nil) {
        Tree('Term, f, OptionalTerm)
      } else {
        error("OptionalTerm with Factor after * or /")
        Nil
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  //SimpleExpression = [Õ-Õ] Term
  //                   {(Õ+Õ | Õ-Õ) Term}.
  // tested 2 + 3 * a * - 2
  def SimpleExpression = {
    println("SimpleExpression")
    val s = sub
    if (s != Nil) {
      inc
      val t = Term
      if (t != Nil) {
        Tree('SimpleExpression, s, Tree('SimpleExpression, t, OptionalSimpleExpression))
      } else {
        error("SimpleExpression with Term")
        Nil
      }
    } else {
      val t = Term
      if (t != Nil) {
        Tree('SimpleExpression, t, OptionalSimpleExpression)
      } else {
        error("SimpleExpression with Term")
        Nil
      }
    }
  }

  def OptionalSimpleExpression: Tree[_] = {
    println("OptionalSimpleExpression")
    val p = plus
    val s = sub
    if (p != Nil) {
      inc
      Tree('SimpleExpression, p, Tree('SimpleExpression, Term, OptionalSimpleExpression))
    }
    if (s != Nil) {
      inc
      Tree('SimpleExpression, s, Tree('SimpleExpression, Term, OptionalSimpleExpression))
    } else {
      // no error because its optional
      Nil
    }
  }

  //Expression       = SimpleExpression
  //                   [(Õ=Õ | Õ#Õ | Õ<Õ |
  //                     Õ<=Õ | Õ>Õ | Õ>=Õ)
  //                    SimpleExpression].
  def Expression = {
    println("Expression")
    val s = SimpleExpression
    if (s != Nil) {
      Tree('Expression, s, OptionalExpression)
    } else {
      error("Expression with SimpleExpression")
      Nil
    }
  }

  def OptionalExpression: Tree[_] = {
    println("OptionalExpression")
    val e = equ
    val s = sharp
    val smal = smaller
    val smaleq = smallereq
    val big = bigger
    val bigeq = biggereq
    var op: Tree[_] = Nil
    if (e != Nil) {
      op = e
    } else if (s != Nil) {
      op = s
    } else if (smal != Nil) {
      op = smal
    } else if (smaleq != Nil) {
      op = smaleq
    } else if (big != Nil) {
      op = big
    } else if (bigeq != Nil) {
      op = bigeq
    }
    if (op != Nil) {
      inc
      val s = SimpleExpression
      if (s != Nil) {
        Tree('Expression, op, OptionalExpression)
      } else {
        error("Expression with OptionalExpression after " + op.value)
        Nil
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  //IndexExpression  = integer | ConstIdent.
  def IndexExpression = {
    println("IndexExpression")
    val i = integer
    if (i != Nil) {
      inc
      Tree('IndexExpression, i)
    } else if (ident != Nil) {
      // no inc because we look forward
      Tree('IndexExpression, ConstIdent)
    } else {
      error("IndexExpression with integer or ConstIdent")
      Nil
    }
  }

  //ConstIdent       = ident.  
  def ConstIdent = {
    println("ConstIdent")
    val id = ident
    if (id != Nil) {
      inc
      Tree('ConsIdent, ident)
    } else {
      error("ConstIdent with ident")
      Nil
    }
  }

  //IdentList = ident {Õ,Õ ident}.
  def IdentList = {
    println("IdentList")
    val id = ident
    if (id != Nil) {
      inc
      Tree('Identlist, id, OptionalIdentList)
    } else {
      error("IdentList with ident")
      Nil
    }
  }

  def OptionalIdentList: Tree[_] = {
    println("OptionalIdentList")
    if (comma) {
      inc
      val id = ident
      if (id != Nil) {
        inc
        Tree('IdentList, OptionalIdentList)
      } else
        Nil
    } else {
      // no error because its optional
      Nil
    }

  }

  //ArrayType = ÕARRAYÕ Õ[Õ IndexExpression Õ]Õ ÕOFÕ Type.
  def ArrayType: Tree[_] = {
    println("ArrayType")
    if (ARRAY) {
      inc
      if (camberedBracketOn) {
        inc
        val i = IndexExpression
        if (camberedBracketOff) {
          inc
          if (OF) {
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
    val idl = IdentList
    if (idl != Nil) {
      if (colon) {
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
    if (RECORD) {
      inc
      val f = FieldList
      if (f != Nil) {
        Tree('RecordType, OptionalFieldList)
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
    if (semicolon) {
      inc
      val f = FieldList
      if (f != Nil) {
        Tree('Fieldlist, FieldList, OptionalFieldList)
      } else
        error("OptionalFieldList with FieldList after ;")
      Nil
    } else {
      // no error because its optional
      Nil
    }
  }

  //Type = ident | ArrayType | RecordType.
  def Type: Tree[_] = {
    println("Type")
    val id = ident
    val arr = ARRAY
    val rec = RECORD
    if (id != Nil) {
      inc
      Tree('Type, id)
    } else if (arr) {
      Tree('Type, ArrayType)
    } else if (RECORD) {
      Tree('Type, RecordType)
    } else {
      error("Type with ident or ARRAY or Record")
      Nil
    }
  }

  //FPSection = [ÕVARÕ] IdentList Õ:Õ Type.
  def FPSection = {
    println("FPSection")
    if (VAR) {
      inc
    }
    val idl = IdentList
    if (colon) {
      inc
      val t = Type
      if (t != Nil) {
        Tree('FPSection, Type)
      } else {
        error("FPSection with Type after :")
        Nil
      }
    } else {
      error("FPSection with : after [VAR]")
      Nil
    }
  }

  //FormalParameters = FPSection {Õ;Õ FPSection}.
  def FormalParameters = {
    println("FormalParamters")
    val fps = FPSection
    if (fps != Nil) {
      Tree('FormalParameters, fps, OptionalFormalParameters)
    } else {
      error("FormalParameters with FPSection")
      Nil
    }
  }

  def OptionalFormalParameters: Tree[_] = {
    println("OptionalFPSection")
    if (semicolon) {
      inc
      val fps = FPSection
      if (fps != Nil) {
        Tree('FormalParameters, fps, OptionalFormalParameters)
      } else {
        error("OptionalFormalParameters with FPSection after ;")
        Nil
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  //ProcedureHeading = ÕPROCEDUREÕ ident Õ(Õ [FormalParameters] Õ)Õ.
  def ProcedureHeading = {
    println("ProcedureHeading")
    if (PROCEDURE) {
      inc
      val id = ident
      if (id != Nil) {
        inc
        if (bracketOn) {
          inc
          if (bracketOff) {
            inc
            Tree('ProcedureHeading, id)
          } else {
            Tree('ProcedureHeading, id, FormalParameters)
          }
        } else {
          error("ProcedureHeading with ( after ident")
          Nil
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

  //ProcedureBody    = Declarations ÕBEGINÕ StatementSequence ÕENDÕ
  def ProcedureBody = {
    println("ProcedureBody")
    val decl = Declarations
    if (decl != Nil) {
      if (BEGIN) {
        inc
        val sts = StatementSequence
        if (sts != Nil) {
          if (END) {
            inc
            Tree('ProcedureBody, sts)
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
    val ph = ProcedureHeading
    if (ph != Nil) {
      if (semicolon) {
        inc
        val pb = ProcedureBody
        if (pb != Nil) {
          val id = ident
          if (id != Nil) {
            inc
            Tree('ProcedureDeclaration, ph, Tree('Body, pb, id))
          } else {
            error("ProcedureDeclaration with ident")
            Nil
          }
        } else {
          error("ProcedureDeclaration with ProcedureBody")
          Nil
        }
      } else {
        error("ProcedureDeclaration with ;")
        Nil
      }
    } else {
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
  def Declarations: Tree[_] = {
    println("Declarations")
    if (CONST) {
      inc
      val d = OptionalConstDeclarations
      if (d != Nil) {
        Tree('Declarations, d, Tree('Declations, Declarations, OptionalProcedureDeclarations))
      } else {
        error("Declarations with OptionalConstDeclrations")
        Nil
      }
    } else if (TYPE) {
      inc
      val d = OptionalTypeDeclarations
      if (d != Nil) {
        Tree('Declarations, d, Tree('Declations, Declarations, OptionalProcedureDeclarations))
      } else {
        error("Declarations with OptionalTypeDeclrations")
        Nil
      }
    } else if (VAR) {
      inc
      val d = OptionalVarDeclarations
      if (d != Nil) {
        Tree('Declarations, d, Tree('Declations, Declarations, OptionalProcedureDeclarations))
      } else {
        error("Declarations with OptionalVarDeclrations")
        Nil
      }
    } else {
      // no error because all is optional
      OptionalProcedureDeclarations
    }
  }

  def OptionalConstDeclarations: Tree[_] = {
    val id = ident
    if (id != Nil) {
      inc
      if (equ != Nil) {
        inc
        val expr = Expression
        if (expr != Nil) {
          if (semicolon) {
            inc
            Tree('Declarations, id, Tree('ConstDeclrations, expr, OptionalConstDeclarations))
          } else {
            error("CONST Declarations with ; after Expression")
            Nil
          }
        } else {
          error("CONST Declarations with Expression after =")
          Nil
        }
      } else {
        error("CONST Declarations with = after ident")
        Nil
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  def OptionalTypeDeclarations: Tree[_] = {
    val id = ident
    if (id != Nil) {
      inc
      if (equ != Nil) {
        inc
        val t = Type
        if (t != Nil) {
          if (semicolon) {
            inc
            Tree('Declarations, id, Tree('TypeDeclrations, t, OptionalTypeDeclarations))
          } else {
            error("TYPE Declarations with ; after Type")
            Nil
          }
        } else {
          error("TYPE Declarations with Type after =")
          Nil
        }
      } else {
        error("TYPE Declarations with = after ident")
        Nil
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  def OptionalVarDeclarations: Tree[_] = {
    val id = ident
    if (id != Nil) {
      val idl = IdentList
      if (colon) {
        inc
        val t = Type
        if (t != Nil) {
          if (semicolon) {
            inc
            Tree('Declarations, idl, Tree('VarDeclrations, t, OptionalVarDeclarations))
          } else {
            error("VAR Declarations with ; after Type")
            Nil
          }
        } else {
          error("VAR Declarations with Type after :")
          Nil
        }
      } else {
        error("VAR Declarations with : after IdentList")
        Nil
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  def OptionalProcedureDeclarations: Tree[_] = {
    if (PROCEDURE) {
      val p = ProcedureDeclaration
      if (p != Nil) {
        if (semicolon) {
          inc
          Tree('OptionalProcedureDeclarations, p, OptionalProcedureDeclarations)
        } else {
          error("OptionalProcedureDeclarations with ; after ProcedureDeclaration")
          Nil
        }
      } else {
        error("OptionalProcedureDeclarations with complete ProcedureDeclaration after PROCEDURE")
        Nil
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  //Module           = ÕMODULEÕ ident Õ;Õ Declarations
  //                   ÕBEGINÕ StatementSequence
  //                   ÕENDÕ ident Õ.Õ.
  def Module = {
    println("Module")
    if (MODULE) {
      inc
      val id = ident
      if (id != Nil) {
        inc
        if (semicolon) {
          inc
          val d = Declarations
          if (BEGIN) {
            inc
            val sts = StatementSequence
            if (sts != Nil) {
              if (END) {
                inc
                val id2 = ident
                if (id2 != Nil) {
                  inc
                  if (DOT) {
                    inc
                    Tree('Module, id, Tree(sts, id2))
                  } else {
                    error("Module with . after ident")
                    Nil
                  }
                } else {
                  error("Module with ident after END")
                  Nil
                }
              } else {
                error("Module with END after StatementSequence")
                Nil
              }
            } else {
              error("Module with StatementSequence after BEGIN")
              Nil
            }
          } else {
            error("Module with BEGIN after Declarations")
            Nil
          }

        } else {
          error("Module with ; after ident")
          Nil
        }
      } else {
        error("Module with ident after MODULE")
        Nil
      }
    } else {
      error("Module with MODULE")
      Nil
    }
  }

  //Assignment        = ident Selector Õ:=Õ Expression.
  def Assignment(id: Tree[_]) = {
    println("Assignment")
    if (id != Nil) {
      val s = Selector
      if (_def != Nil) {
        inc
        val expr = Expression
        if (expr != Nil) {
          Tree('Assignment, s, expr)
        } else {
          error("Assignment with Expression after Selector")
          Nil
        }
      } else {
        error("Assignment with Selector after :=")
        Nil
      }
    } else {
      error("Assignment with ident")
      Nil
    }
  }

  //ActualParameters  = Expression {Õ,Õ Expression}.
  def ActualParameters = {
    println("ActualParameters")
    val expr = Expression
    if (expr != Nil) {
      Tree('ActualParameters, expr, OptionalExpresion)
    } else {
      error("ActualParameters with Expression")
      Nil
    }
  }

  def OptionalExpresion: Tree[_] = {
    if (DOT) {
      inc
      val expr = Expression
      if (expr != Nil) {
        Tree('Expression, expr, OptionalExpresion)
      } else {
        error("OptionalExpresion with Expression")
        Nil
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  //ProcedureCall = ident Õ(Õ [ActualParameters] Õ)Õ.
  def ProcedureCall(id: Tree[_]) = {
    println("ProcedureCall")
    if (id != Nil) {
      if (bracketOn) {
        inc
        if (bracketOff) {
          inc
          Tree('ProcedureCall, id)
        } else {
          val a = ActualParameters;
          if (bracketOff) {
            inc
            Tree('ProcedureCall, id, a)
          } else {
            error("ProcedureCall ends with )")
            Nil
          }
        }
      } else {
        error("ProcedureCall with (")
        Nil
      }
    } else {
      error("ProcedureCall with ident")
      Nil
    }
  }

  //IfStatement = ÕIFÕ Expression ÕTHENÕ StatementSequence
  //	{ÕELSIFÕ Expression ÕTHENÕ StatementSequence}
  //  	[ÕELSEÕ StatementSequence] ÕENDÕ.
  def IfStatement: Tree[_] = {
    println("IfStatement")
    if (IF) {
      inc
      val expr = Expression
      if (expr != Nil) {
        if (THEN) {
          inc
          val sts = StatementSequence
          if (sts != Nil) {
            val elseif = OptionalELSIF
            if (ELSE) {
              inc
              val sts2 = StatementSequence
              if (sts2 != Nil) {
                if (END) {
                  inc
                  Tree('IfStatement, expr, Tree('StatementSequence, sts, Tree('Else, elseif, sts2)))
                } else {
                  error("IfStatement with END after StatementSequence")
                  Nil
                }
              } else {
                error("IfStatement with StatementSequence after ELSE")
                Nil
              }
            } else {
              if (END){
                inc
                Tree('IfStatement, expr, Tree('StatementSequence, sts, Tree('Else, elseif)))
              }
              else{
                error("IfStatement with END after StatementSequence")
                Nil
              }
            }
          } else {
            error("IfStatement with StatementSequence after THEN")
            Nil
          }
        } else {
          error("IfStatement with THEN after Expression")
          Nil
        }
      } else {
        error("IfStatement with Expression after IF")
        Nil
      }
    } else {
      error("IfStatement with If")
      Nil
    }
  }

  def OptionalELSIF: Tree[_] = {
    if (ELSEIF) {
      inc
      val expr = Expression
      if (expr != Nil) {
        if (THEN) {
          inc
          val sts = StatementSequence
          if (sts != Nil) {
            Tree('ELSIF, expr, Tree('StatementSequence, StatementSequence, OptionalELSIF))
          } else {
            error("OptionalELSIF with StatementSequence after THEN")
            Nil
          }
        } else {
          error("OptionalELSIF with THEN after Expression")
          Nil
        }
      } else {
        error("OptionalELSIF with Expression after ELSEIF")
        Nil
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  //WhileStatement = ÕWHILEÕ Expression ÕDOÕ StatementSequence ÕENDÕ.
  def WhileStatement = {
    println("WhileStatement")
    if (WHILE) {
      inc
      val expr = Expression
      if (expr != Nil) {
        if (DO) {
          inc
          val sts = StatementSequence
          if (sts != Nil) {
            if (END) {
              inc
              Tree('WhileStatement, expr, sts)
            } else {
              error("WhileStatement with END after StatementSequence")
              Nil
            }
          } else {
            error("WhileStatement with StatementSequence after DO ")
            Nil
          }
        } else {
          error("WhileStatement with DO after Expression")
          Nil
        }
      } else {
        error("WhileStatement with Expression after WHILE")
        Nil
      }
    } else {
      error("WhileStatement with WHILE")
      Nil
    }
  }

  //RepeatStatement = ÕREPEATÕ StatementSequence ÕUNTILÕ Expression.
  def RepeatStatement = {
    println("RepeatStatement")
    if (REPEAT) {
      inc
      val sts = StatementSequence
      if (sts != Nil) {
        if (UNTIL) {
          inc
          Tree('RepeatStatement, sts, Expression)
        } else {
          error("RepeatStatement with UNTIL after StatementSequence")
          Nil
        }
      } else {
        error("RepeatStatement with StatementSequence after REPEAT")
        Nil
      }
    } else {
      error("RepeatStatement with REPEAT")
      Nil
    }
  }

  //Statement = [Assignment | ProcedureCall |
  //   IfStatement | ÕPRINTÕ Expression |
  //   WhileStatement | RepeatStatement].
  def Statement: Tree[_] = {
    println("Statement")
    val assignOrProccall = ident
    val ifst = IF
    val p = PRINT
    val whi = WHILE
    val r = REPEAT
    if (assignOrProccall != Nil) {
      // we have to look two symbols forward
      val id = assignOrProccall
      inc
      if (bracketOn) {
        Tree('Statement, ProcedureCall(id))
      } else if (_def != Nil) {
        Tree('Statement, Assignment(id))
      } else {
        error("Statement with ( or := after ident")
        Nil
      }
    } else if (ifst) {
      Tree('Statement, IfStatement)
    } else if (p) {
      inc
      Tree('Statement, Expression)
    } else if (whi) {
      Tree('Statement, WhileStatement)
    } else if (r) {
      Tree('Statement, RepeatStatement)
    } else {
      // no error because its optional
      Nil
    }
  }

  //StatementSequence = Statement {Õ;Õ Statement}.
  def StatementSequence: Tree[_] = {
    println("StatementSequence")
    Tree('StatementSequence, Statement, OptionalStatementSequence)      
  }
  
  def OptionalStatementSequence: Tree[_] = {
    if (semicolon){
      inc
      val sts = Statement
      if (sts != Nil){
        Tree('StatementSequence, sts, OptionalStatementSequence)  
      }
      else{
        error("StatementSequence with another StatementSequence after ;")
        Nil
      }
    }
    else{
      // no error because its optional
      Nil
    }
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

  def urparserWithBlanks = {
    while (!scanner.isEOF) {
      val s: Symbol = scanner.next_token()
      if (!scanner.isEOF) {
        println(s.toLongString);
      }
    }
  }

  def urparser: Unit = {
    val s = next
    if (s != None) {
      println(s.get.toLongString);
      urparser
    }
  }

  def checkPrimitive(token: String) = {
    //    println(current.get.token + " == " + token)
    if (current.isEmpty)
      false
    else if (current.get.token == token) {
      println(token)
      true
    } else {
      false
    }
  }

  def checkPrimitiveTree(token: String) = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == token) {
      println(token + " " + current.get.value.getOrElse(""))
      Tree(current)
    } else {
      Nil
    }
  }

  def println(s: Any) = {
    if (current == None)
      System.out.println(s)
    else
      System.out.println(Symbol.linecolumn(current.get.line, current.get.column) + " " + s)
  }

  import java.io.FileReader
  def oberonScanner(path: String) = new OberonScanner(new FileReader(path))

  def error(expectedToken: String) {
    if (current.isEmpty)
      Console.err.println( " Error => " + current + " not expected")
    else {
      Console.err.println(Symbol.linecolumn(current.get.line, current.get.column) + " Error => " + current.get.token + " not expected")
      Console.err.println(Symbol.linecolumn(current.get.line, current.get.column) + " We expect:" + expectedToken)
    }
    System.exit(-1)
  }

  def test(t: Tree[_]) = {
    if (!current.isEmpty) {
      error("No Token")
      Nil
    } else
      t
  }
}

