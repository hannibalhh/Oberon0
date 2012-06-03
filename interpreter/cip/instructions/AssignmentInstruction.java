package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

public class AssignmentInstruction extends AbstractInstruction implements
		Instruction {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private int length;

	public AssignmentInstruction(int fl) {
		length = fl;
	};

	public int getLength() {
		return length;
	}

	public void interpreter() {
		Instruction op1, op2;
		int i1, i2;

		int j = 0, size;

		size = length;

		op1 = Interpreter.computationStack.pop(); // intinstr
		i1 = ((IntegerVal) op1).getIntVal();
		while (j < size) {
			op2 = Interpreter.computationStack.pop(); // intinstr
			myPrintln(op2.toString());
			i2 = ((IntegerVal) op2).getIntVal();
			myPrintln("addr(" + i1 + ") := " + i2);
			Interpreter.runTimeStack.set(i1 + size - (j + 1),
					new IntegerVal(i2));
			j++;
		}
		Interpreter.programCounter++;
	}

	public void print() {
		myPrintln(Interpreter.programCounter + " Interpret ASSIGN " + length);
		Interpreter.programCounter++;
	}

}
