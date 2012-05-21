package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class SetSP extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	public SetSP() {
	};

	public void interpreter() {

		int val = ((IntegerVal) Interpreter.computationStack.pop()).getIntVal();

		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret SetSP");
		if (Interpreter.SP == 0) {
			int i;
			i = 0;
			while (i < val) {
				Interpreter.runTimeStack.add(new IntegerVal(0));
				i++;
			}
		}
		Interpreter.SP = val;
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret SetSP");
		Interpreter.programCounter++;
	}
}
