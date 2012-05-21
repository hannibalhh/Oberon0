package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.descriptor.SimpleTypeDescr;
import cip.instructions.CharVal;

public class CharNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	char charVal;

	public CharNode() {
		charVal = 0;
	};

	public CharNode(char fc) {
		charVal = fc;
	};

	public void setCharVal(char fc) {
		charVal = fc;
	};

	public char getCharVal() {
		return charVal;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		trace("CharNode " + charVal);
		CodeGen.outInstr(new CharVal(charVal));
		unindent();
		return new SimpleTypeDescr("char");
	};

	public void print() {
		trace("CharNode " + charVal);
		unindent();
	};
}
