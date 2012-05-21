package cip.astnodes;

import java.util.HashMap;

import cip.base.*;
import cip.descriptor.*;
import cip.exceptions.CompileException;
import cip.instructions.*;

public class ContNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	AbstractNode address;

	public ContNode() {
		address = null;
	};

	public ContNode(AbstractNode fn) {
		address = fn;
	};

	public void setAddress(AbstractNode fn) {
		address = fn;
	};

	public AbstractNode getAddress() {
		return address;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{/* ? */
		AbstractDescr e;

		AbstractDescr t = new SimpleTypeDescr("integer");// assume integer!!!!
		trace("ContNode ");

		if ((address instanceof IdfNode) || (address instanceof ArrayRefNode)
				|| (address instanceof RecordRefNode)) {
			t = address.compile(symbolTable);
			if (address instanceof IdfNode) {
				e = CodeGen.search(((IdfNode) address).getIdName(),((IdfNode) address).getLine());
				if (!(e instanceof IntConstDescr))
					CodeGen.outInstr(new ContInstruction(t.getSize()));
			} else
				CodeGen.outInstr(new ContInstruction(t.getSize()));
		}
		unindent();
		return t;
	};

	public void print() {
		trace("ContNode ");
		address.print();
		unindent();
	};
}
