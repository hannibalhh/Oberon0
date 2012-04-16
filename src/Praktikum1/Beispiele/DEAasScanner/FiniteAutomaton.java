package Praktikum1.Beispiele.DEAasScanner;

import java.io.BufferedReader;
import java.io.InputStreamReader;

public class FiniteAutomaton {
	
	public static void main(String[] args) {
     
	 byte delta[][] = new byte[4][2];
	 
	 final byte a = 0;
     final byte b = 1;
     
     byte token;
     
     String buffer;
     byte pos;
     
     final byte ende = 3;
     final byte start = 0;
     
     byte state = start;
     
     delta [0][a] = 0;
     delta [0][b] = 1;
     delta [1][a] = 0;
     delta [1][b] = 2;
     delta [2][a] = 0;
     delta [2][b] = 3;
     delta [3][a] = 3;
     delta [3][b] = 3;
     
     BufferedReader In = new BufferedReader(
 			new InputStreamReader(System.in));

     System.out.println("Ein Scanner für (a+b)*bbb(a+b)*");
     System.out.println();
//     buffer = MyIO.readString("Please enter expression to scan: ", In);
//     
//     for (pos = 0; pos < buffer.length(); pos++)
//      {
//       token = (byte) (buffer.charAt(pos)-'a');
//       state = delta[state][token];
//      }
//     if (state != ende)
//      System.out.print("Nicht ");
//     System.out.println("akzeptiert");
 	 }

}
