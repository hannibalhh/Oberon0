package Praktikum2.cip.base;

import java.io.Serializable;
import java.util.HashMap;

/**
 * Die abstrakte Darstellung eines Knotens im abstrakten Syntaxbaum.
 * 
 * @author VLL + nilo
 * 
 */
public abstract class AbstractNode
        implements
		Serializable {

	private static final long serialVersionUID = 1L;

	int line, column;

	public AbstractNode() {
		line = -1;
		column = -1;
	};

	abstract public AbstractDescr compile(
			HashMap<String, AbstractDescr> symbolTable);

	abstract public void print();

	/**
	 * Diese Variable steuert die Einr�ckung bei der Ausgabe
	 */
	private static String spaces = "";

	/**
	 * Diese Methode ist eine Darstellungshilfe f�r den abstrakten Syntaxbaum.
	 * Die Einr�ckung wird aufgehoben.
	 */
	public void unindent() {
		spaces = spaces.substring(2);
	}

	private void indent() {
		spaces = spaces + "  ";
	}

	/**
	 * Einr�ckungstiefe um zwei Leerzeichen erh�hen und den String ausgeben.
	 * 
	 * @param fieldName
	 */
	public void trace(String s) {
		this.indent();
//		if (Debug.debug > 0) System.out.println(spaces + s);
	}
}
