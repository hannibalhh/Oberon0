
package beispiel4;

%%

%{ /* interner Code für den Scanner */

	public static void show(String strg) {
		System.out.println(strg);
	}
%}

%line		/* zählt Zeilen in yyline, startet mit 0 */
%state AnwTeil

%class Scanner4

digit	= 	[0-9]
integer	= 	{digit}+
operator = 	[+*/-]
rowComment = 	"//"[^\r\n]*
whiteSpace = 	[ \t\b]
BEGIN	=	"BEGIN"{whiteSpace}
END	= 	"END"{whiteSpace}*

%%

^{BEGIN}		{ show ("<<");
			  yybegin(AnwTeil);
			}

<AnwTeil> {
	^{END}		{ show(">>"); }
	{integer}	{ show (yytext()); }
	{operator}	{ show (yytext()); }
}

^{rowComment}		{}

{whiteSpace} 		{}

[a-z]$			{ show ("Buchstabe '"+yytext()+"' am Ende der Zeile "+Integer.toString(yyline+1)); }
[^\r\n]			{show("ungueltiges Zeichen '"+yytext()+"'");}
\n			{ show ("# "+Integer.toString(yyline+2)); }
\r 			{ /* überlesen wegen Windows */ }
