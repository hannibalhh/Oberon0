package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

public class LabelInstruction extends AbstractInstruction implements
		Instruction {

	private static final long serialVersionUID = 1L;

	int labelVal;

	public LabelInstruction(int fl) {
		labelVal = fl;
	};

	public int getLabelVal() {
		return labelVal;
	};

	public void setlabelVal(int fl) {
		labelVal = fl;
	};

	public void interpreter() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret LabelVal " + labelVal);
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret LabelVal " + labelVal);
		Interpreter.programCounter++;
	}
}
