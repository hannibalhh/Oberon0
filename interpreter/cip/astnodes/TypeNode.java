package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.exceptions.CompileException;

public class TypeNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	IdfNode typeName;
	AbstractNode type;

	public TypeNode() {
		typeName = null;
		type = null;
	};

	public TypeNode(IdfNode fi, AbstractNode fv) {
		typeName = fi;
		type = fv;
	};

	public void setTypeName(IdfNode fi) {
		typeName = fi;
	};

	public void setType(AbstractNode fv) {
		type = fv;
	};

	public IdfNode getTypeName() {
		return typeName;
	};

	public AbstractNode getType() {
		return type;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		AbstractDescr d = null;

		trace("TypeNode ");
		d = type.compile(symbolTable);
		symbolTable.put(typeName.getIdName(), d);
		unindent();
		return null;
	};

	public void print() {
		trace("TypeNode ");
		typeName.print();
		type.print();
		unindent();
	};
}
