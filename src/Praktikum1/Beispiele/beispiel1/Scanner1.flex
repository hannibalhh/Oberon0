package beispiel1;
%%
%public
%standalone
%class Scanner1

digit	= 	[0-9]
digits	= 	[0-9]*
int	= 	{digit}+
operator = 	[+*/-]
blank	=	[ ]
rowComment = 	"//"[^\r\n]*
%%
{blank}*	{}
x v {System.out.println("x v");}
xy {System.out.println("xy");}
{digit}		{System.out.println ("digit: "+yytext());}
{digits}	{System.out.println ("digits: "+yytext());}
{int}		{System.out.println ("int: "+yytext());}
{int}{operator}	{System.out.println ("int op: "+yytext());}
{int}{blank}+	{System.out.println ("int bl: "+yytext());}
-{digits}	{System.out.println ("negZahl: "+yytext());}
{operator}	{System.out.println ("operator: "+yytext());}
^{rowComment}	{System.out.println ("rowComment <"+yytext()+">");}
ax*[a|bc]	{System.out.println (": "+yytext());}
\n 		{}
\r		{}
. 		{System.out.println ("komisches Zeichen "+yytext());}

