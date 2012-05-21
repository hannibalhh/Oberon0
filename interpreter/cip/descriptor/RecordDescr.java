package cip.descriptor;

import java.util.*;

import cip.base.AbstractDescr;

public class RecordDescr extends AbstractDescr {

	private static final long serialVersionUID = 1L;

	int addr; // start address of first field
	HashMap<String, AbstractDescr> recsymbolTable;

	public RecordDescr() {
		size = 0;
		addr = 0;
		recsymbolTable = new HashMap<String, AbstractDescr>();
	}

	public RecordDescr(int fs, HashMap<String, AbstractDescr> fr) {
		size = fs;
		recsymbolTable = fr;
		addr = 0;
	}

	public void setAddr(int fa) {
		addr = fa;
	}

	public int getAddr() {
		return addr;
	}

	public void setRecSymbolTable(HashMap<String, AbstractDescr> fr) {
		recsymbolTable = fr;
	}

	public HashMap<String, AbstractDescr> getRecsymbolTable() {
		return recsymbolTable;
	}
	public void print(){
		trace("RecordDescr: size: " + size + "level: " + level);
		Iterator<AbstractDescr> it = recsymbolTable.values().iterator();
		while (it.hasNext()) it.next().print();
		unindent();
	}
}