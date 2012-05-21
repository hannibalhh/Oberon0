package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class ReduceStack extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	int length;

	public ReduceStack(int fl) {
		length = fl;
	};

	public int getLength() {
		return length;
	}

	public void interpreter() {
		int i = 0, last;
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret ReduceStack " + length);
		last = Interpreter.runTimeStack.size();
		while (i < length) {
			Interpreter.runTimeStack.remove(last - 1 - i);
			i++;
		}
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret ReduceStack " + length);
		Interpreter.programCounter++;
	}
}