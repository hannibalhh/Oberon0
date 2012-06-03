package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

public class StopInstruction extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	public StopInstruction() {
	};

	public void interpreter()

	{
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret StopInstruction");
		System.out.println("------------------------ stop");
		Interpreter.programCounter++;
	};

	public void print()

	{
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret StopInstruction");
		Interpreter.programCounter++;
	}
}
