package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

public class StringVal extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	String sVal;

	public StringVal(String fs) {
		sVal = fs;
	};

	public String getSVal() {
		return sVal;
	};

	public void interpreter() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret StrInstr " + sVal);
		Interpreter.computationStack.push(this);
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret StrInstr " + sVal);
		Interpreter.programCounter++;
	}

	@Override
	public String toString() {
		return "StringVal(sVal=" + sVal + ")";
	}
	
	
}
