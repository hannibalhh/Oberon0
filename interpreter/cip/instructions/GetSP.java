package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class GetSP extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	public GetSP() {
	};

	public void interpreter() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret GetSP " + Interpreter.SP);
		Interpreter.computationStack.push(new IntegerVal(Interpreter.SP));
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret GetSP");
		Interpreter.programCounter++;
	}
}