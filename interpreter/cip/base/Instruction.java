package cip.base;


/**
 * Das Interface <code>Instruction</code> beschreibt die notwendigen Methoden, die eine Instruktion enthalten muss.
 * @author nilo
 *
 */
public interface Instruction{
	/**
	 * 
	 */
	public void interpreter();
	/**
	 * Steuert die Ausgebe einer Instruktion
	 */
	public void print();
	/**
	 * Diese Methode wird durch die <code>AbsractDescr</code> implementiert und muss von den konkreten Instructions nicht implementiert werden, wenn diese auch <code>AbstractDescr</code> erweitern. 
	 * @return Operator
	 */
}