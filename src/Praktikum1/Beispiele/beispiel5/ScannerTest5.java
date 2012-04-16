package Praktikum1.Beispiele.beispiel5;
//import java.io.InputStreamReader;
import java.io.IOException;
public class ScannerTest5 {
	public static void main	(String args[]){
		Yylex scanner = new Yylex(System.in);
		try {System.out.println("Auf geht's Beispiel 5 ");
			scanner.yylex();
		} catch (IOException e){
			System.err.println(e);
		}
	}
}
