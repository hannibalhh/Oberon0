package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode; //import cip.base.Operator;
import cip.descriptor.RecordDescr;
import cip.exceptions.CompileException;

public class RecordNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	FieldListNode fields;

	public RecordNode() {
		fields = null;
	};

	public RecordNode(FieldListNode ff) {
		fields = ff;
	}

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		RecordDescr d = new RecordDescr();
		trace("RecordNode ");
		if (fields != null)
			fields.compile(d, symbolTable);
		unindent();
		return d;
	};

	public void print() {
		trace("RecordNode ");
		if (fields != null)
			fields.print();
		unindent();
	};
}
