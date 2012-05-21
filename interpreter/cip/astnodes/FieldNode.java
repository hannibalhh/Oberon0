package cip.astnodes;

import java.util.ArrayList;
import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen;
import cip.descriptor.RecordDescr;
import cip.descriptor.VarDescr;
import cip.exceptions.CompileException;

public class FieldNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	ArrayList<IdfNode> fieldNames;
	AbstractNode fieldType;

	public FieldNode() {
		fieldNames = null;
		fieldType = null;
	};

	public FieldNode(ArrayList<IdfNode> fl, AbstractNode ft) {
		fieldNames = fl;
		fieldType = ft;
	};

	public void setFieldNames(ArrayList<IdfNode> fl) {
		fieldNames = fl;
	};

	public void setFieldType(IdfNode ft) {
		fieldType = ft;
	};

	public ArrayList<IdfNode> getFieldNames() {
		return fieldNames;
	};

	public AbstractNode getFieldtype() {
		return fieldType;
	};

	public AbstractDescr compile(RecordDescr fd,
			HashMap<String, AbstractDescr> symbolTable) throws CompileException {
		trace("FieldNode ");
		int i;
		int addr = 0;
		int laddr = fd.getAddr();
		int lsize = fd.getSize();
		VarDescr entry = null;
		AbstractDescr d = null;

		if (fieldNames != null) {
			if (fieldType instanceof ArrayNode)
				d = ((ArrayNode) fieldType).compile(symbolTable);
			else if (fieldType instanceof RecordNode)
				d = ((RecordNode) fieldType).compile(symbolTable);
			else
				d = CodeGen.search(((IdfNode) (fieldType)).getIdName(),((IdfNode) (fieldType)).getLine());

			for (i = 0; i < fieldNames.size(); i++) {
				addr = laddr;
				entry = new VarDescr(addr, d);
				laddr = laddr + d.getSize();
				lsize = lsize + d.getSize();

				fd.getRecsymbolTable().put(
						((IdfNode) (fieldNames.get(i))).getIdName(), entry);
			}
		}
		fd.setAddr(laddr);
		fd.setSize(lsize);
		unindent();
		return d;
	};

	public void print() {
		trace("FieldNode ");
		int i;
		if (fieldNames != null)
			for (i = 0; i < fieldNames.size(); i++) {
				fieldNames.get(i).print();
			}
		fieldType.print();
		unindent();
	}

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		return null;
	};
}
