package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

public class ContInstruction extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	private int length;

	public ContInstruction(int fl) {
		length = fl;
	};

	public int getLength() {
		return length;
	}

	public void interpreter()

	{
		IntegerVal i;

		int addr, size, j = 0;

		size = length;
		addr = ((IntegerVal) (Interpreter.computationStack.pop())).getIntVal();
		while (j < size) {
			i = (IntegerVal) Interpreter.runTimeStack.get(addr + j);
			Interpreter.computationStack.push(i);
			myPrint(Interpreter.programCounter + " ");
			myPrintln("Interpret ContInstruction Addr = " + addr + " value = "
					+ i.getIntVal());
			j++;
		}
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret ContInstruction " + length);
		Interpreter.programCounter++;
	}
}
