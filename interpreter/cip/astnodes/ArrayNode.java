package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.descriptor.ArrayDescr;
import cip.descriptor.IntConstDescr;
import cip.exceptions.CompileException;

public class ArrayNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	AbstractNode numberOfElements;
	AbstractNode basetype;

	public ArrayNode() {
		numberOfElements = null;
		basetype = null;
	};

	public ArrayNode(AbstractNode fl, AbstractNode fb) {
		numberOfElements = fl;
		basetype = fb;
	}

	public void setNumberelems(AbstractNode fl) {
		numberOfElements = fl;
	};

	public AbstractNode getNumberelems() {
		return numberOfElements;
	};

	public void setBasetype(AbstractNode fb) {
		basetype = fb;
	};

	public AbstractNode getBasetype() {
		return basetype;
	};

	public ArrayDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException {
		AbstractDescr basedescr = null;
		int numelem;

		if (numberOfElements instanceof IntNode)
			numelem = ((IntNode) numberOfElements).getIntVal();
		else
			numelem = ((IntConstDescr) CodeGen
					.search(((IdfNode) numberOfElements).getIdName(),
					((IdfNode) numberOfElements).getLine()))
					.getIntVal();
		trace("ArrayNode " + numelem);
		if (basetype instanceof ArrayNode) {
			basedescr = ((ArrayNode) basetype).compile(symbolTable);
		} else if (basetype instanceof RecordNode) {
			basedescr = ((RecordNode) basetype).compile(symbolTable);
		} else {
			basedescr = CodeGen.search(((IdfNode) basetype).getIdName(),
					((IdfNode) basetype).getLine());
		}
		unindent();
		return new ArrayDescr(numelem, basedescr.getSize() * numelem, basedescr);
	}

	public void print() {
		trace("ArrayNode ");
		numberOfElements.print();
		basetype.print();
		unindent();
	}
}