package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class SetSL extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	public SetSL() {
	};

	public void interpreter() {
		int level, wert;
		level = ((IntegerVal) Interpreter.computationStack.pop()).getIntVal();
		wert = ((IntegerVal) Interpreter.computationStack.pop()).getIntVal();
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret SetSL" + level + " " + wert);
		Interpreter.SL[level] = wert;
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret SetSL");
		Interpreter.programCounter++;
	}
}
