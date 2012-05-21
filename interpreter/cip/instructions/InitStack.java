package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class InitStack extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	int length;

	public InitStack(int fl) {
		length = fl;
	};

	public int getLength() {
		return length;
	}

	public void interpreter() {
		int i = 0;
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret InitStack");
		while (i < length) {
			Interpreter.runTimeStack.add(new IntegerVal(0));
			i++;
		}
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret InitStack " + length);
		Interpreter.programCounter++;
	}
}
