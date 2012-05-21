package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class GetFP extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	public GetFP() {
	};

	public void interpreter() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret GetFP " + Interpreter.FP);
		Interpreter.computationStack.push(new IntegerVal(Interpreter.FP));
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret GetFP ");
		Interpreter.programCounter++;
	}
}