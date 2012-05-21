package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

public class BoolVal extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	boolean boolVal;

	public BoolVal(boolean fb) {
		boolVal = fb;
	};

	public boolean getBoolVal() {
		return boolVal;
	};

	public void setBoolVal(boolean fb) {
		boolVal = fb;
	};

	public void interpreter() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret BoolVal " + boolVal);
		Interpreter.computationStack.push(this);
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret BoolVal " + boolVal);
		Interpreter.programCounter++;
	}
}
