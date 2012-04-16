package beispiel6;

class Scanner6 {
    // Kennzeichen für token
    public static void main(String argv[]) throws java.io.IOException {
    	   System.out.println("Auf gehts Beispiel 6: ");	
	Yylex yy = new Yylex(System.in);
	Yytoken t;
	while ((t = yy.yylex()) != null) {
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
  public static final int ICONST = 2;
%}

%% 
","	{ return new Yytoken(KOMMA,yytext()); }
[0-9]+ 	{ return new Yytoken(ICONST,yytext()); }

[^\r\n]	{}
\r	{}
\n	{}


