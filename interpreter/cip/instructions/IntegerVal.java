package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

/**
 * Ein Integerwert.
 * 
 * @author nilo
 * 
 */
public class IntegerVal extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	int intVal;

	public IntegerVal(int fi) {
		intVal = fi;
	};

	public int getIntVal() {
		return intVal;
	};

	public void setIntVal(int fi) {
		intVal = fi;
	};

	public void interpreter() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret IntVal " + intVal);
		Interpreter.computationStack.push(this);
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret IntVal " + intVal);
		Interpreter.programCounter++;
	}

	@Override
	public String toString() {
		return "IntegerVal(intVal=" + intVal + ")";
	}
	
	
}
