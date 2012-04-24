package Praktikum2
import scala.annotation.tailrec

@tailrec
object OberonParser extends App {
  import java.io.FileReader
  import Praktikum2._
  import Praktikum2.OberonScanner._
  import Tree._

  val scanner = new OberonScanner(new FileReader("src/Examples/NT/Term"))
  var current = next
  def parser = Module
  
  println(Term)

  // terminals
  // operators
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
  // terminals with values
  def ident = checkPrimitive(Token.ident)
  def string = checkPrimitive(Token.string)
  def integer = checkPrimitive(Token.integer)
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

  // NonTermonals with Precondition:
  // current token is NOT used

  //Selector         = {Õ.Õ ident | Õ[Õ Expression Õ]Õ}.
  def Selector: Tree[_] = {
    println("Selector")
    if (DOT != Nil) {
      inc
      val id = ident
      if (id != Nil)
        Tree('Selector, ident)
      else {
        error("Selector with ident after dot")
        Nil
      }
    } else if (edgeBracketOn != Nil) {
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
    } else if (bracketOn != Nil) {
      inc
      val expr = Expression
      inc
      if (bracketOff == Nil) {
        error("Factor ends with )")
        Nil
      } else {
        inc
        Tree('Factor, expr)
      }
    } else if (READ != Nil) {
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
    if (r != Nil) {
      inc
      Tree('Read, r, Prompt)
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
  // 
  def Term = {
    println("Term")
    val f = Factor
    if (f != Nil) {
      inc
      Tree('Term, f, OptionalTerm)
    } else
      error("Term with Factor")
    Nil
  }

  def OptionalTerm: Tree[_] = {
    println("OptionalTerm")
    if (mul != Nil || div != Nil) {
      inc
      val f = Factor
      if (f != Nil) {
        inc
        Tree('Term, f, OptionalTerm)
      } else {
        error("OptionalTerm with Factor after * or /")
        Nil
      }
    } else
      // no error because its optional
      Nil
  }

  //SimpleExpression = [Õ-Õ] Term
  //                   {(Õ+Õ | Õ-Õ) Term}.
  def SimpleExpression = {
    println("SimpleExpression")
    val s = sub
    if (s != Nil) {
      inc
      val t = Term
      if (t != Nil) {
        inc
        Tree('SimpleExpression, s, Tree('SimpleExpression, Term, OptionalSimpleExpression))
      } else {
        error("SimpleExpression with Term")
        Nil
      }
    } else {
      val t = Term
      if (t != Nil) {
        inc
        Tree('SimpleExpression, Term, OptionalSimpleExpression)
      } else {
        error("SimpleExpression with Term")
        Nil
      }
    }
  }

  def OptionalSimpleExpression: Tree[_] = {
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
      inc
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
        inc
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
      Tree('Identlist, id, OptionalIdentList)
    } else {
      error("IdentList with ident")
      Nil
    }
  }

  def OptionalIdentList: Tree[_] = {
    println("OptionalIdentList")
    if (DOT != Nil) {
      val id = ident
      if (id != Nil) {
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
    if (RECORD != Nil) {
      inc
      val f = FieldList
      if (f != Nil) {
        inc
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
    if (semicolon != Nil) {
      inc
      val f = FieldList
      if (f != Nil) {
        Tree('Fieldlist, FieldList, OptionalFieldList)
      } else
        error("OptionalFieldList with FieldList after ;")
      Nil
    } else
      // no error because its optional
      Nil
  }

  //Type = ident | ArrayType | RecordType.
  def Type: Tree[_] = {
    println("Type")
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
    if (VAR != Nil) {
      inc
    }
    val idl = IdentList
    inc
    if (colon != Nil) {
      inc
      val t = Type
      if (t != Nil) {
        inc
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
      inc
      Tree('FormalParameters, fps, OptionalFormalParameters)
    } else {
      error("FormalParameters with FPSection")
      Nil
    }
  }

  def OptionalFormalParameters: Tree[_] = {
    println("OptionalFPSection")
    if (semicolon != Nil) {
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
    if (PROCEDURE != Nil) {
      inc
      val id = ident
      if (id != Nil) {
        inc
        if (bracketOn != Nil) {
          inc
          if (bracketOff == Nil) {
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

  //ProcedureBody    = Declarations ÕBEGINÕ StatementSequence ÕENDÕ ident Õ.Õ.
  def ProcedureBody = {
    println("ProcedureBody")
    val decl = Declarations
    if (decl != Nil) {
      inc
      if (BEGIN != Nil) {
        inc
        val sts = StatementSequence
        if (sts != Nil) {
          inc
          if (END != Nil) {
            inc
            val id = ident
            if (id != Nil) {
              inc
              if (DOT != Nil) {
                inc
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
    val ph = ProcedureHeading
    if (ph != Nil) {
      inc
      if (semicolon != Nil) {
        inc
        val pb = ProcedureBody
        if (pb != Nil) {
          inc
          if (ident != Nil) {
            Tree('ProcedureDeclaration, ph, pb)
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
    if (CONST != Nil) {
      inc
      val d = OptionalConstDeclarations
      if (d != Nil) {
        Tree('Declarations, d, Tree('Declations, Declarations, OptionalProcedureDeclarations))
      } else {
        error("Declarations with OptionalConstDeclrations")
        Nil
      }
    } else if (TYPE != Nil) {
      inc
      val d = OptionalTypeDeclarations
      if (d != Nil) {
        Tree('Declarations, d, Tree('Declations, Declarations, OptionalProcedureDeclarations))
      } else {
        error("Declarations with OptionalTypeDeclrations")
        Nil
      }
    } else if (VAR != Nil) {
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
      Nil
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
          inc
          if (semicolon != Nil) {
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
          inc
          if (semicolon != Nil) {
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
    val idl = IdentList
    if (idl != Nil) {
      inc
      if (colon != Nil) {
        inc
        val t = Type
        if (t != Nil) {
          inc
          if (semicolon != Nil) {
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
    val p = ProcedureDeclaration
    if (p != Nil) {
      inc
      if (semicolon != Nil) {
        Tree('OptionalProcedureDeclarations, p, OptionalProcedureDeclarations)
      } else {
        error("OptionalProcedureDeclarations with ; after ProcedureDeclaration")
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
    if (MODULE != Nil) {
      inc
      val id = ident
      if (id != Nil) {
        inc
        if (semicolon != Nil) {
          inc
          val d = Declarations
          if (d != Nil) {
            inc
            if (BEGIN != Nil) {
              inc
              val sts = StatementSequence
              if (sts != Nil) {
                inc
                if (END != Nil) {
                  inc
                  val id2 = ident
                  if (id2 != Nil) {
                    inc
                    if (DOT != Nil) {
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
            error("Module with Declarations after ;")
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
  def Assignment = {
    println("Assignment")
    val id = ident
    if (id != Nil) {
      inc
      val s = Selector
      if (s != Nil) {
        inc
        if (_def != Nil) {
          inc
          val expr = Expression
          if (expr != Nil) {
            inc
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
        error("Assignment with := after ident")
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
      inc
      Tree('ActualParameters, expr, OptionalExpresion)
    }
    Nil
  }

  def OptionalExpresion: Tree[_] = {
    if (DOT != Nil) {
      inc
      val expr = Expression
      if (expr != Nil) {
        Tree('Expression, expr, OptionalExpresion)
      } else {
        error("OptionalExpresion with Expression")
        Nil
      }
    } else {
      Nil
    }
  }

  //ProcedureCall = ident Õ(Õ [ActualParameters] Õ)Õ.
  def ProcedureCall = {
    println("ProcedureCall")
    val id = ident
    if (id != Nil) {
      inc
      if (bracketOn != Nil) {
        inc
        if (bracketOff != Nil) {
          Tree('ProcedureCall, id)
        } else {
          Tree('ProcedureCall, id, ActualParameters)
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
    if (IF != Nil) {
      val expr = Expression
      if (expr != Nil) {
        inc
        if (THEN != Nil) {
          inc
          val sts = StatementSequence
          if (sts != Nil) {
            inc
            val elseif = OptionalELSIF
            if (ELSE != Nil) {
              inc
              val sts2 = StatementSequence
              if (sts2 != Nil) {
                inc
                if (END != Nil) {
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
              Tree('IfStatement, expr, Tree('StatementSequence, sts, Tree('Else, elseif)))
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
    if (ELSEIF != Nil) {
      inc
      val expr = Expression
      if (expr != Nil) {
        inc
        if (THEN != Nil) {
          inc
          val sts = StatementSequence
          if (sts != Nil) {
            inc
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
      error("OptionalELSIF with ELSEIF")
      Nil
    }
  }

  //WhileStatement = ÕWHILEÕ Expression ÕDOÕ StatementSequence ÕENDÕ.
  def WhileStatement = {
    println("WhileStatement")
    if (WHILE != Nil) {
      inc
      val expr = Expression
      if (expr != Nil) {
        inc
        if (DO != Nil) {
          inc
          val sts = StatementSequence
          if (sts != Nil) {
            inc
            if (END != Nil) {
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
    if (REPEAT != Nil) {
      inc
      val sts = StatementSequence
      if (sts != Nil) {
        inc
        if (UNTIL != Nil) {
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
    // !! ERROR !!
    val assign = ident
    val proccall = ident
    // 
    val ifst = IF
    val p = PRINT
    val whi = WHILE
    val r = REPEAT
    if (assign != Nil) {
      Tree('Statement, Assignment)
    } else if (proccall != Nil) {
      Tree('Statement,ProcedureCall)
    } else if (ifst != Nil) {
      Tree('Statement, IfStatement)
    } else if (p != Nil) {
      inc
      Tree('Statement, Expression)
    } else if (whi != Nil) {
      Tree('Statement, WhileStatement)
    } else if (r != Nil) {
      Tree('Statement, RepeatStatement)
    } else {
      Nil
    }
  }

  //StatementSequence = Statement {Õ;Õ Statement}.
  def StatementSequence: Tree[_] = {
    println("StatementSequence")
    val sts = Statement
    if (sts != Nil){
      inc
      if (semicolon != Nil){
        inc
        Tree('StatementSequence,sts,StatementSequence)
      }
    }
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

//  def urparser = {
//    while (!scanner.isEOF) {
//      val s: Symbol = scanner.next_token()
//      if (!scanner.isEOF) {
//        println(s.toLongString);
//      }
//    }
//  }
  
  def urparser: Unit = {
    val s = next
    if (s != None) {
      println(s.get.toLongString);
      urparser
    }
  }

  def checkPrimitive(token: String) = {
    if (current.isEmpty)
      Nil
    else if (current.get.token == token)
      Tree(current)
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

