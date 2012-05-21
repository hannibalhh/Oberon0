package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

public class RealVal extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	float rVal;

	public RealVal(float fr) {
		rVal = fr;
	};

	public float getRVal() {
		return rVal;
	};

	public void setRVal(float fr) {
		rVal = fr;
	};

	public void interpreter() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret RealVal " + rVal);
		Interpreter.computationStack.push(this);
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret RealVal " + rVal);
		Interpreter.programCounter++;
	}
}
