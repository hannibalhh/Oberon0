package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.descriptor.SimpleTypeDescr;
import cip.instructions.StringVal;

public class StringNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	String stringVal;

	public StringNode() {
		stringVal = "";
	};

	public StringNode(String fs) {
		stringVal = fs;
	};

	public void setStringval(String fs) {
		stringVal = fs;
	};

	public String getStringVal() {
		return stringVal;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		trace("CharNode " + stringVal);
		CodeGen.outInstr(new StringVal(stringVal));
		unindent();
		return new SimpleTypeDescr("string");
	};

	public void print() {
		trace("CharNode " + stringVal);
		unindent();
	};
}
