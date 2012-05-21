package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

public class PrintInstruction extends AbstractInstruction implements
		Instruction {

	private static final long serialVersionUID = 1L;

	public PrintInstruction() {
	};

	public void interpreter()

	{
		Instruction i;

		i = Interpreter.computationStack.pop();
		if (i instanceof IntegerVal)
			System.out.println("" + ((IntegerVal) i).getIntVal());
		else if (i instanceof StringVal)
			System.out.println(((StringVal) i).getSVal());
		Interpreter.programCounter++;
	};

	@Override
	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret PrintInstruction");
		Interpreter.programCounter++;
	}
}