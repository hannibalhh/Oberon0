package Praktikum2.cip.descriptor;

import java.util.*;

import Praktikum2.cip.base.AbstractDescr;

public class RecordDescr extends AbstractDescr {

	private static final long serialVersionUID = 1L;

	HashMap<String, AbstractDescr> recsymbolTable;

	public RecordDescr() {
		size = 0;
		recsymbolTable = new HashMap<String, AbstractDescr>();
	}

	public RecordDescr(int fs, HashMap<String, AbstractDescr> fr) {
		size = fs;
		recsymbolTable = fr;
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