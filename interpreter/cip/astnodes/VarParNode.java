package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode; //import cip.base.Operator;
import cip.descriptor.SimpleTypeDescr;
import cip.exceptions.CompileException;

public class VarParNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	AbstractNode name;

	public VarParNode() {
		name = null;
	};

	public VarParNode(AbstractNode fn) {
		name = fn;
	};

	public void setName(AbstractNode fn) {
		name = fn;
	};

	public AbstractNode getName() {
		return name;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{

		AbstractDescr t = new SimpleTypeDescr("integer");// assume integer!!!!
		trace("VarParNode ");

		if ((name instanceof IdfNode) || (name instanceof ArrayRefNode)
				|| (name instanceof RecordRefNode)) {
			t = name.compile(symbolTable);

		} else
			name.compile(symbolTable);

		unindent();
		return t;
	};

	public void print() {
		trace("VarParNode ");
		name.print();
		unindent();
	};
}
