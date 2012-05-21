package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

public class CharVal extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	char cVal;;

	public CharVal(char fc) {
		cVal = fc;
	};

	public char getcVal() {
		return cVal;
	};

	public void setCVal(char fc) {
		cVal = fc;
	};

	public void interpreter() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret CharVal " + cVal);
		Interpreter.computationStack.push(this);
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret CharVal " + cVal);
		Interpreter.programCounter++;
	}
}
