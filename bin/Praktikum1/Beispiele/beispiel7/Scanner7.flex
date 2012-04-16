package beispiel7;

import java.util.ArrayList;
import java.util.List;

class Scanner7 {
    // Kennzeichen für token
    public static void main(String argv[]) throws java.io.IOException {
    System.out.println("Auf gehts Beispiel 7: ");	
	Yylex yy = new Yylex(System.in);
	Yytoken t;
	while ( (t = yy.yylex()) != null ) {
	    System.out.println(t.token+"\t"+t.lexem);
	}
    }
}

class Yytoken {
  public int token;
  public String lexem;
  Yytoken (int aToken, String aLexem) {
	token = aToken;
	lexem = new String(aLexem);
  }
}

%%

%{
  public static final int KOMMA = 1;
  public static final int ID = 20;
  public static final int ICONST = 30;

  static List schlWort = new ArrayList();

  public int isSchlWort(String text) {
    if (schlWort.contains(text)) return schlWort.indexOf(text);
    else return -1;
  }

%}

%init{
	schlWort = new ArrayList();
	schlWort.add("begin");
%init}


digit =	[0-9]
letter=	[a-zA-Z]
id =	{letter}({letter}|{digit}|"_"})*

%% 
","		{ return new Yytoken(KOMMA, yytext()); }
{digit}+ 	{ return new Yytoken(ICONST, yytext()); }
{id} 		{ int i = isSchlWort(yytext());
	  	  if (i>=0) i=101+i;
	  	  else i=ID;
	  	  return new Yytoken(i, yytext()); 
		}

[^\r\n]		{}
\r		{}
\n		{}


