package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.descriptor.RecordDescr;
import cip.exceptions.CompileException;
import cip.instructions.AdditionInstruction;

public class RecordRefNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	AbstractNode record;
	FieldIdNode field;

	public RecordRefNode() {
		record = null;
		field = null;
	};

	public RecordRefNode(AbstractNode fv, FieldIdNode ff) {
		record = fv;
		field = ff;
	};

	public void setRecord(AbstractNode fv) {
		record = fv;
	};

	public void setField(FieldIdNode ff) {
		field = ff;
	};

	public AbstractNode getRecord() {
		return record;
	};

	public FieldIdNode getField() {
		return field;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		RecordDescr r;
		AbstractDescr a;
		trace("RecordRefNode ");
		r = (RecordDescr) record.compile(symbolTable);
		a = field.compile(r.getRecsymbolTable());
		CodeGen.outInstr(new AdditionInstruction());
		unindent();
		return a;
	};

	public void print() {
		trace("RecordRefNode ");
		record.print();
		field.print();
		unindent();
	};
}
