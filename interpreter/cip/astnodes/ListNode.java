package cip.astnodes;

import java.util.ArrayList;
import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.exceptions.CompileException;

public class ListNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	ArrayList<AbstractNode> l;

	public ListNode() {
	}

	public ListNode(ArrayList<AbstractNode> fl) {
		l = fl;
	}

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		int i = 0;
		AbstractNode e;

		trace("Listnode");
		while (i < l.size()) {
			e = (l.get(i++));
			if (e != null)
				e.compile(symbolTable);
		}
		unindent();
		return null;
	}

	public void print() {
		int i = 0;
		AbstractNode e;

		trace("Listnode");
		while (i < l.size()) {
			e = (l.get(i++));
			if (e != null)
				e.print();
		}
		unindent();
	}
}
