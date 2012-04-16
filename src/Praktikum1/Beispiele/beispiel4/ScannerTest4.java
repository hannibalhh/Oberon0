package Praktikum1.Beispiele.beispiel4;
//import java.io.InputStreamReader;
import java.io.IOException;
public class ScannerTest4 {
	public static void main	(String args[]){
		Scanner4 scanner = new Scanner4(System.in);
		System.out.println("Auf gehts: ");
		try {
			scanner.yylex();
		} catch (IOException e){
			System.err.println(e);
		}
	}
}
