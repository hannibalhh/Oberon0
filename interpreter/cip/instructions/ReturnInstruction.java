package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class ReturnInstruction extends AbstractInstruction implements
		Instruction {

	private static final long serialVersionUID = 1L;

	public ReturnInstruction() {
	};

	public void interpreter() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret ReturnInstruction " + Interpreter.RK);
		Interpreter.programCounter = Interpreter.RK;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret ReturnInstruction ");
		Interpreter.programCounter++;
	}
}
