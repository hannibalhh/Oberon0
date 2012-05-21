package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class SetRK extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	public SetRK() {
	};

	public void interpreter() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret SetRK");
		Interpreter.RK = ((IntegerVal) Interpreter.computationStack.pop())
				.getIntVal();
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret SetRK");
		Interpreter.programCounter++;
	}
}
