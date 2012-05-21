package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen;
import cip.instructions.ReadInstruction;

public class ReadNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	StringNode prompt = null;

	public ReadNode(StringNode fn) {
		prompt = fn;
	};

	public StringNode GetPrompt() {
		return prompt;
	}

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		String s = "";
		if (prompt != null)
			s = prompt.getStringVal();
		trace("ReadNode ");
		CodeGen.outInstr(new ReadInstruction(s));
		unindent();
		return null;
	}

	public void print() {
		trace("ReadNode ");
		unindent();
	};
}
