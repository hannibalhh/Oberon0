package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class GetRK extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	public GetRK() {
	};

	public void interpreter() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret GetRK " + Interpreter.RK);
		Interpreter.computationStack.push(new IntegerVal(Interpreter.RK));
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret GetRK ");
		Interpreter.programCounter++;
	}
}
