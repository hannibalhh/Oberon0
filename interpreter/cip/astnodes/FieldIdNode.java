package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.descriptor.VarDescr;
import cip.instructions.IntegerVal;

public class FieldIdNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	String fieldName;

	public FieldIdNode() {
		fieldName = "";
	};

	public FieldIdNode(String fs) {
		fieldName = fs;
	};

	public void setFieldName(String fs) {
		fieldName = fs;
	};

	public String getFieldName() {
		return fieldName;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		VarDescr e;

		trace("FieldIdNode " + fieldName);
		e = (VarDescr) symbolTable.get(fieldName);
		CodeGen.outInstr(new IntegerVal(((VarDescr) e).getAddr()));
		unindent();
		return e.getTyp();
	}

	public void print() {
		trace("FieldIdNode " + fieldName);
		unindent();
	};
}