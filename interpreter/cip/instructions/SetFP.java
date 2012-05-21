package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class SetFP extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	public SetFP() {
	};

	public void interpreter() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret SetFP");
		Interpreter.FP = ((IntegerVal) Interpreter.computationStack.pop())
				.getIntVal();
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret SetFP");
		Interpreter.programCounter++;
	}
}
