package Praktikum4
import cip.base.Instruction
import cip.instructions._

object OberonInstructions {
  def newLabel = cip.base.CodeGen.newLabel
  def AdditionInstruction = {
	val i = new AdditionInstruction	  
	out(i)
    trace("ADD")
  }

  def AssignmentInstruction(int:Int) = {
    val i = new AssignmentInstruction(int)	  
	out(i)
    trace("ASSIGN, " + (i.getLength))
  }

  def BoolVal(b:Boolean) = {
    val i = new BoolVal(b)	  
	out(i)
    trace("PUSHB, " + i.getBoolVal)
  }

  def BranchFalseInstruction(int:Int) = {
    val i = new BranchFalseInstruction(int)	  
	out(i)
    trace("BF, " + i.getDestination)
  }

  def BranchTrueInstruction(int:Int) = {
    val i = new BranchTrueInstruction(int)	  
	out(i)
    trace("BT, " + i.getDestination)
  }

  def CallInstruction(int:Int) = {
    val i = new CallInstruction(int)	  
	out(i)
    trace("CALL, " + i.getDestination)
  }

  def CharVal(c:Char) = {
    val i = new CharVal(c)	  
	out(i)
    trace("PUSHC, " + i.getcVal)
  }

  def ContInstruction(int:Int)  = {
    val i = new ContInstruction(int)	  
	out(i)
    trace("CONT, " + i.getLength)
  }

  def DivisionInstruction = {
    val i = new DivisionInstruction	  
	out(i)
    trace("DIV")
  }

  def EqualsInstruction = {
    val i = new EqualsInstruction	  
	out(i)
    trace("EQ")
  }

  def GetFP = {
    val i = new GetFP	  
	out(i)
    trace("GETFP")
  }

  def GetRK = {
    val i = new GetRK	  
	out(i)
    trace("GETRK")
  }

  def GetSL = {
    val i = new GetSL	  
	out(i)
    trace("GETSL")
  }

  def GetSP = {
    val i = new GetSP	  
	out(i)
    trace("GETSP")
  }

  def GreaterEqualThanInstruction = {
    val i = new GreaterEqualThanInstruction	  
	out(i)
    trace("GE")
  }

  def GreaterThanInstruction = {
    val i = new GreaterThanInstruction	  
	out(i)
    trace("GT")
  }

  def InitStack(int:Int) = {
    val i = new InitStack(int)	  
	out(i)
    trace("INIT, " + i.getLength)
  }

  def IntegerVal(int:Int) = {
    val i = new IntegerVal(int)	  
	out(i)
    trace("PUSHI, " + i.getIntVal)
  }

  def JumpInstruction(int:Int) = {
    val i = new JumpInstruction(int)	  
	out(i)
    trace("JMP, " + i.getDestination)
  }

  def LessEqualThanInstruction = {
    val i = new LessEqualThanInstruction	  
	out(i)
    trace("LE")
  }

  def LabelInstruction(int:Int) = {
    val i = new LabelInstruction(int)	  
	out(i)
    trace("LABEL, " + i.getLabelVal)
  }

  def LessThanInstruction = {
    val i = new LessThanInstruction	  
	out(i)
    trace("LT")
  }

  def MultiplicationInstruction = {
    val i = new MultiplicationInstruction	  
	out(i)
    trace("MUL")
  }

  def NotEqualInstruction = {
    val i = new NotEqualInstruction	  
	out(i)
    trace("NEQ")
  }

  def PrintInstruction = {
    val i = new PrintInstruction	  
	out(i)
    trace("PRINT")
  }

  def RealVal(float:Float) = {
    val i = new RealVal(float)	  
	out(i)
    trace("PUSHF, " + i.getRVal)
  }

  def ReduceStack(int:Int) = {
    val i = new ReduceStack(int)	  
	out(i)
    trace("REDUCE, " + i.getLength)
  }

  def ReturnInstruction = {
    val i = new ReturnInstruction	  
	out(i)
    trace("RET")
  }

  def SetFP = {
    val i = new SetFP	  
	out(i)
    trace("SETFP")
  }

  def SetRK = {
    val i = new SetRK	  
	out(i)
    trace("SETRK")
  }

  def SetSL = {
    val i = new SetSL	  
	out(i)
    trace("SETSL")
  }

  def SetSP = {
    val i = new SetSP	  
	out(i)
    trace("SETSP")
  }

  def StringVal(str:String) = {
    val i = new StringVal(str)	  
	out(i)
    trace("PUSHS, " + i.getSVal)
  }

  def SubtractionInstruction = {
    val i = new SubtractionInstruction	  
	out(i)
    trace("SUB")
  }

  def ReadInstruction(str:String)  = {
    val i = new ReadInstruction(str)	  
	out(i)
    val prompt = i.getPrompt
    if (prompt.length > 0)
      trace("READ, " + prompt)
    else
      trace("READ")
  }

  def PushRegisterInstruction(str:String) = {
    val i = new PushRegisterInstruction(str)	  
	out(i)
    trace("PUSHREG, " + i.getRegister)
  }

  def PopRegisterInstruction(str:String) = {
    val i = new PopRegisterInstruction(str)	  
	out(i)
    trace("POPREG, " + i.getRegister)
  }

  def StopInstruction = {
    val i = new StopInstruction	  
	out(i)
    trace("STOP")
  }

  def trace(s: String) = if(OberonDebug.instructions) println("Instruction: " + s + " is written")
  def out(i: Instruction) = cip.base.CodeGen.outInstr(i)
}