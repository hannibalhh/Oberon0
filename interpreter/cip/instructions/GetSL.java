package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class GetSL extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	public GetSL() {
	};

	public void interpreter() {
		int level, wert;
		level = ((IntegerVal) Interpreter.computationStack.pop()).getIntVal();
		wert = Interpreter.SL[level];
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret GetSL" + level + " " + wert);
		Interpreter.computationStack.push(new IntegerVal(wert));
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret GetSL");
		Interpreter.programCounter++;
	}
}
