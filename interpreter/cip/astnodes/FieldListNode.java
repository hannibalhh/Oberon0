package cip.astnodes;

import java.util.ArrayList;
import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode; //import cip.base.Operator;
import cip.descriptor.RecordDescr;
import cip.exceptions.CompileException;

public class FieldListNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	ArrayList<FieldNode> fields;

	public FieldListNode() {
		fields = null;
	};

	public FieldListNode(ArrayList<FieldNode> fl) {
		fields = fl;
	};

	public void setFields(ArrayList<FieldNode> fl) {
		fields = fl;
	};

	public ArrayList<FieldNode> getFields() {
		return fields;
	};

	public void compile(RecordDescr d,
			HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		trace("FieldListNode ");
		int i;

		if (fields != null)
			for (i = 0; i < fields.size(); i++) {
				fields.get(i).compile(d, symbolTable);
			}
		unindent();
	};

	public void print() {
		trace("FieldListNode ");
		int i;

		if (fields != null)
			for (i = 0; i < fields.size(); i++) {
				fields.get(i).print();
			}
		unindent();
	}

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		return null;
	};
}
