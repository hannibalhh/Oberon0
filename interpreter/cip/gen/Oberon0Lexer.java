/* The following code was generated by JFlex 1.4.3 on 25.01.12 10:08 */

// Hier stehen package und import Deklarationen

package cip.gen;
  

/**
 * This class is a scanner generated by 
 * <a href="http://www.jflex.de/">JFlex</a> 1.4.3
 * on 25.01.12 10:08 from the specification file
 * <tt>D:/user/eclipse-workspaces/eclipse-revo/Oberon0/oberon0.flex</tt>
 */
public class Oberon0Lexer {

  /** This character denotes the end of file */
  public static final int YYEOF = -1;

  /** initial size of the lookahead buffer */
  private static final int ZZ_BUFFERSIZE = 16384;

  /** lexical states */
  public static final int YYINITIAL = 0;

  /**
   * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
   * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
   *                  at the beginning of a line
   * l is of the form l = 2*k, k a non negative integer
   */
  private static final int ZZ_LEXSTATE[] = { 
     0, 0
  };

  /** 
   * Translates characters to character classes
   */
  private static final String ZZ_CMAP_PACKED = 
    "\11\0\1\1\1\6\2\0\1\1\22\0\1\1\1\0\1\71\1\62"+
    "\3\0\1\70\1\52\1\53\1\55\1\54\1\65\1\56\1\4\1\5"+
    "\12\2\1\57\1\61\1\63\1\60\1\64\2\0\1\23\1\27\1\16"+
    "\1\11\1\15\1\32\1\30\1\33\1\31\2\3\1\14\1\7\1\17"+
    "\1\12\1\26\1\3\1\24\1\20\1\21\1\13\1\22\1\34\1\35"+
    "\1\25\1\3\1\66\1\0\1\67\3\0\1\36\1\46\1\47\1\11"+
    "\1\42\1\32\1\43\1\50\1\37\2\3\1\45\1\7\1\40\1\10"+
    "\1\26\1\3\1\44\1\51\1\41\1\13\1\22\1\34\1\35\1\25"+
    "\1\3\uff85\0";

  /** 
   * Translates characters to character classes
   */
  private static final char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

  /** 
   * Translates DFA states to action switch labels.
   */
  private static final int [] ZZ_ACTION = zzUnpackAction();

  private static final String ZZ_ACTION_PACKED_0 =
    "\1\1\1\2\1\1\1\3\1\4\1\5\1\6\30\4"+
    "\1\7\1\10\1\11\1\12\1\13\1\14\1\15\1\16"+
    "\1\17\1\20\1\21\1\22\1\23\1\24\2\2\1\25"+
    "\1\1\1\4\1\26\1\27\1\30\21\4\1\31\11\4"+
    "\1\32\1\33\1\34\2\0\1\35\1\36\1\4\1\37"+
    "\3\4\1\40\2\4\1\41\3\4\1\42\1\43\21\4"+
    "\1\44\2\4\1\45\1\46\1\4\1\47\1\4\1\50"+
    "\1\51\1\52\3\4\1\53\11\4\1\54\1\4\1\55"+
    "\2\4\1\56\1\57\1\60\1\61\4\4\1\62\1\63"+
    "\2\4\1\64\1\65\3\4\1\66\1\67\1\70\1\71"+
    "\5\4\1\72\1\4\1\73\1\4\1\74\1\75\1\4"+
    "\1\76\1\77";

  private static int [] zzUnpackAction() {
    int [] result = new int[182];
    int offset = 0;
    offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAction(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /** 
   * Translates a state to a row index in the transition table
   */
  private static final int [] ZZ_ROWMAP = zzUnpackRowMap();

  private static final String ZZ_ROWMAP_PACKED_0 =
    "\0\0\0\72\0\164\0\256\0\350\0\72\0\u0122\0\u015c"+
    "\0\u0196\0\u01d0\0\u020a\0\u0244\0\u027e\0\u02b8\0\u02f2\0\u032c"+
    "\0\u0366\0\u03a0\0\u03da\0\u0414\0\u044e\0\u0488\0\u04c2\0\u04fc"+
    "\0\u0536\0\u0570\0\u05aa\0\u05e4\0\u061e\0\u0658\0\u0692\0\72"+
    "\0\72\0\72\0\72\0\72\0\u06cc\0\72\0\72\0\72"+
    "\0\u0706\0\u0740\0\72\0\72\0\72\0\u077a\0\u07b4\0\u07ee"+
    "\0\u0828\0\u0862\0\350\0\350\0\350\0\u089c\0\u08d6\0\u0910"+
    "\0\u094a\0\u0984\0\u09be\0\u09f8\0\u0a32\0\u0a6c\0\u0aa6\0\u0ae0"+
    "\0\u0b1a\0\u0b54\0\u0b8e\0\u0bc8\0\u0c02\0\u0c3c\0\350\0\u0c76"+
    "\0\u0cb0\0\u0cea\0\u0d24\0\u0d5e\0\u0d98\0\u0dd2\0\u0e0c\0\u0e46"+
    "\0\72\0\72\0\72\0\u0e80\0\u07b4\0\72\0\u0eba\0\u0eba"+
    "\0\350\0\u0ef4\0\u0f2e\0\u0f68\0\350\0\u0fa2\0\u0fdc\0\350"+
    "\0\u1016\0\u1050\0\u108a\0\350\0\350\0\u10c4\0\u10fe\0\u1138"+
    "\0\u1172\0\u11ac\0\u11e6\0\u1220\0\u125a\0\u1294\0\u12ce\0\u1308"+
    "\0\u1342\0\u137c\0\u13b6\0\u13f0\0\u142a\0\u1464\0\72\0\u149e"+
    "\0\u14d8\0\350\0\350\0\u1512\0\350\0\u154c\0\350\0\350"+
    "\0\350\0\u1586\0\u15c0\0\u15fa\0\350\0\u1634\0\u166e\0\u16a8"+
    "\0\u16e2\0\u171c\0\u1756\0\u1790\0\u17ca\0\u1804\0\350\0\u183e"+
    "\0\350\0\u1878\0\u18b2\0\350\0\350\0\350\0\350\0\u18ec"+
    "\0\u1926\0\u1960\0\u199a\0\350\0\350\0\u19d4\0\u1a0e\0\350"+
    "\0\350\0\u1a48\0\u1a82\0\u1abc\0\350\0\350\0\350\0\350"+
    "\0\u1af6\0\u1b30\0\u1b6a\0\u1ba4\0\u1bde\0\350\0\u1c18\0\350"+
    "\0\u1c52\0\350\0\350\0\u1c8c\0\350\0\350";

  private static int [] zzUnpackRowMap() {
    int [] result = new int[182];
    int offset = 0;
    offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackRowMap(String packed, int offset, int [] result) {
    int i = 0;  /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int high = packed.charAt(i++) << 16;
      result[j++] = high | packed.charAt(i++);
    }
    return j;
  }

  /** 
   * The transition table of the DFA
   */
  private static final int [] ZZ_TRANS = zzUnpackTrans();

  private static final String ZZ_TRANS_PACKED_0 =
    "\1\2\1\3\1\4\1\5\1\6\1\7\1\3\1\10"+
    "\1\11\1\12\1\11\1\13\1\14\1\15\1\16\1\17"+
    "\1\5\1\20\1\21\1\22\1\23\1\5\1\24\1\25"+
    "\1\5\1\26\1\27\1\5\1\30\1\5\1\31\1\32"+
    "\1\33\1\20\1\15\1\5\1\34\1\14\1\35\1\36"+
    "\1\5\1\37\1\40\1\41\1\42\1\43\1\44\1\45"+
    "\1\46\1\47\1\50\1\51\1\52\1\53\1\54\1\55"+
    "\1\56\1\57\73\0\1\3\4\0\1\3\65\0\1\4"+
    "\1\0\1\60\67\0\2\5\3\0\43\5\25\0\1\61"+
    "\66\0\2\5\3\0\1\5\1\62\1\5\1\62\37\5"+
    "\22\0\2\5\3\0\15\5\1\63\5\5\1\64\11\5"+
    "\1\63\5\5\22\0\2\5\3\0\1\5\1\65\1\5"+
    "\1\65\16\5\1\66\5\5\1\66\12\5\22\0\2\5"+
    "\3\0\10\5\1\67\20\5\1\67\11\5\22\0\2\5"+
    "\3\0\1\5\1\70\1\5\1\70\37\5\22\0\2\5"+
    "\3\0\5\5\1\71\2\5\1\72\15\5\1\73\2\5"+
    "\1\72\4\5\1\71\4\5\22\0\2\5\3\0\1\5"+
    "\1\74\1\5\1\74\37\5\22\0\2\5\3\0\1\5"+
    "\1\75\1\5\1\75\37\5\22\0\2\5\3\0\15\5"+
    "\1\76\1\77\5\5\1\100\10\5\1\76\3\5\1\100"+
    "\1\5\22\0\2\5\3\0\14\5\1\101\12\5\1\101"+
    "\13\5\22\0\2\5\3\0\10\5\1\102\4\5\1\103"+
    "\13\5\1\102\3\5\1\103\5\5\22\0\2\5\3\0"+
    "\6\5\1\104\24\5\1\104\7\5\22\0\2\5\3\0"+
    "\15\5\1\105\17\5\1\105\5\5\22\0\2\5\3\0"+
    "\6\5\1\106\24\5\1\106\7\5\22\0\2\5\3\0"+
    "\23\5\1\107\17\5\22\0\2\5\3\0\1\5\1\110"+
    "\1\5\1\110\1\111\7\5\1\112\12\5\1\112\13\5"+
    "\22\0\2\5\3\0\24\5\1\113\14\5\1\113\1\5"+
    "\22\0\2\5\3\0\15\5\1\103\17\5\1\103\5\5"+
    "\22\0\2\5\3\0\23\5\1\107\5\5\1\114\11\5"+
    "\22\0\2\5\3\0\1\5\1\75\1\5\1\75\4\5"+
    "\1\102\20\5\1\102\11\5\22\0\2\5\3\0\6\5"+
    "\1\104\24\5\1\115\7\5\22\0\2\5\3\0\1\5"+
    "\1\116\4\5\1\106\24\5\1\106\7\5\22\0\2\5"+
    "\3\0\1\5\1\74\1\5\1\74\35\5\1\117\1\5"+
    "\22\0\2\5\3\0\32\5\1\120\10\5\100\0\1\121"+
    "\71\0\1\122\71\0\1\123\11\0\6\124\1\0\63\124"+
    "\71\125\1\126\2\0\1\60\67\0\6\61\1\0\63\61"+
    "\2\0\2\5\3\0\2\5\1\127\1\130\37\5\22\0"+
    "\2\5\3\0\13\5\1\131\27\5\22\0\2\5\3\0"+
    "\12\5\1\132\17\5\1\132\10\5\22\0\2\5\3\0"+
    "\1\5\1\133\1\5\1\133\37\5\22\0\2\5\3\0"+
    "\11\5\1\134\30\5\1\134\22\0\2\5\3\0\2\5"+
    "\1\135\40\5\22\0\2\5\3\0\22\5\1\136\5\5"+
    "\1\136\12\5\22\0\2\5\3\0\10\5\1\137\20\5"+
    "\1\137\11\5\22\0\2\5\3\0\12\5\1\140\17\5"+
    "\1\140\10\5\22\0\2\5\3\0\4\5\1\141\36\5"+
    "\22\0\2\5\3\0\17\5\1\142\23\5\22\0\2\5"+
    "\3\0\6\5\1\143\24\5\1\143\7\5\22\0\2\5"+
    "\3\0\15\5\1\144\17\5\1\144\5\5\22\0\2\5"+
    "\3\0\2\5\1\145\40\5\22\0\2\5\3\0\15\5"+
    "\1\146\17\5\1\146\5\5\22\0\2\5\3\0\7\5"+
    "\1\147\2\5\1\150\1\5\1\151\2\5\1\152\7\5"+
    "\1\151\2\5\1\150\5\5\1\147\2\5\22\0\2\5"+
    "\3\0\1\5\1\153\1\5\1\153\16\5\1\154\5\5"+
    "\1\154\12\5\22\0\2\5\3\0\21\5\1\155\12\5"+
    "\1\155\6\5\22\0\2\5\3\0\15\5\1\156\17\5"+
    "\1\156\5\5\22\0\2\5\3\0\10\5\1\157\20\5"+
    "\1\157\11\5\22\0\2\5\3\0\5\5\1\160\30\5"+
    "\1\160\4\5\22\0\2\5\3\0\22\5\1\161\5\5"+
    "\1\161\12\5\22\0\2\5\3\0\32\5\1\162\10\5"+
    "\22\0\2\5\3\0\7\5\1\147\2\5\1\150\1\5"+
    "\1\151\2\5\1\152\7\5\1\163\2\5\1\150\5\5"+
    "\1\147\2\5\22\0\2\5\3\0\1\5\1\164\41\5"+
    "\22\0\2\5\3\0\27\5\1\165\13\5\22\0\2\5"+
    "\3\0\35\5\1\166\5\5\110\0\1\167\3\0\2\5"+
    "\3\0\4\5\1\170\36\5\22\0\2\5\3\0\22\5"+
    "\1\171\5\5\1\171\12\5\22\0\2\5\3\0\17\5"+
    "\1\172\23\5\22\0\2\5\3\0\6\5\1\173\13\5"+
    "\1\174\5\5\1\174\2\5\1\173\7\5\22\0\2\5"+
    "\3\0\12\5\1\175\17\5\1\175\10\5\22\0\2\5"+
    "\3\0\11\5\1\176\30\5\1\176\22\0\2\5\3\0"+
    "\6\5\1\177\24\5\1\177\7\5\22\0\2\5\3\0"+
    "\6\5\1\200\24\5\1\200\7\5\22\0\2\5\3\0"+
    "\10\5\1\201\20\5\1\201\11\5\22\0\2\5\3\0"+
    "\14\5\1\202\12\5\1\202\13\5\22\0\2\5\3\0"+
    "\1\5\1\203\1\5\1\203\37\5\22\0\2\5\3\0"+
    "\4\5\1\204\36\5\22\0\2\5\3\0\2\5\1\205"+
    "\40\5\22\0\2\5\3\0\6\5\1\206\24\5\1\206"+
    "\7\5\22\0\2\5\3\0\7\5\1\207\30\5\1\207"+
    "\2\5\22\0\2\5\3\0\10\5\1\210\20\5\1\210"+
    "\11\5\22\0\2\5\3\0\22\5\1\211\5\5\1\211"+
    "\12\5\22\0\2\5\3\0\25\5\1\212\15\5\22\0"+
    "\2\5\3\0\7\5\1\213\30\5\1\213\2\5\22\0"+
    "\2\5\3\0\11\5\1\214\30\5\1\214\22\0\2\5"+
    "\3\0\5\5\1\215\30\5\1\215\4\5\22\0\2\5"+
    "\3\0\33\5\1\216\7\5\22\0\2\5\3\0\2\5"+
    "\1\205\33\5\1\217\4\5\22\0\2\5\3\0\36\5"+
    "\1\220\4\5\22\0\2\5\3\0\35\5\1\221\5\5"+
    "\22\0\2\5\3\0\30\5\1\222\12\5\22\0\2\5"+
    "\3\0\5\5\1\223\30\5\1\223\4\5\22\0\2\5"+
    "\3\0\5\5\1\224\30\5\1\224\4\5\22\0\2\5"+
    "\3\0\23\5\1\225\17\5\22\0\2\5\3\0\12\5"+
    "\1\226\17\5\1\226\10\5\22\0\2\5\3\0\16\5"+
    "\1\227\24\5\22\0\2\5\3\0\15\5\1\230\17\5"+
    "\1\230\5\5\22\0\2\5\3\0\15\5\1\231\17\5"+
    "\1\231\5\5\22\0\2\5\3\0\14\5\1\232\12\5"+
    "\1\232\13\5\22\0\2\5\3\0\6\5\1\233\24\5"+
    "\1\233\7\5\22\0\2\5\3\0\12\5\1\234\17\5"+
    "\1\234\10\5\22\0\2\5\3\0\10\5\1\235\20\5"+
    "\1\235\11\5\22\0\2\5\3\0\27\5\1\236\13\5"+
    "\22\0\2\5\3\0\12\5\1\237\17\5\1\237\10\5"+
    "\22\0\2\5\3\0\6\5\1\240\24\5\1\240\7\5"+
    "\22\0\2\5\3\0\6\5\1\241\24\5\1\241\7\5"+
    "\22\0\2\5\3\0\34\5\1\242\6\5\22\0\2\5"+
    "\3\0\33\5\1\243\7\5\22\0\2\5\3\0\31\5"+
    "\1\244\11\5\22\0\2\5\3\0\6\5\1\245\24\5"+
    "\1\245\7\5\22\0\2\5\3\0\2\5\1\246\40\5"+
    "\22\0\2\5\3\0\10\5\1\247\20\5\1\247\11\5"+
    "\22\0\2\5\3\0\12\5\1\250\17\5\1\250\10\5"+
    "\22\0\2\5\3\0\2\5\1\251\40\5\22\0\2\5"+
    "\3\0\15\5\1\252\17\5\1\252\5\5\22\0\2\5"+
    "\3\0\22\5\1\253\5\5\1\253\12\5\22\0\2\5"+
    "\3\0\33\5\1\254\7\5\22\0\2\5\3\0\27\5"+
    "\1\255\13\5\22\0\2\5\3\0\34\5\1\256\6\5"+
    "\22\0\2\5\3\0\4\5\1\257\36\5\22\0\2\5"+
    "\3\0\2\5\1\260\40\5\22\0\2\5\3\0\1\5"+
    "\1\261\1\5\1\261\37\5\22\0\2\5\3\0\35\5"+
    "\1\262\5\5\22\0\2\5\3\0\31\5\1\263\11\5"+
    "\22\0\2\5\3\0\15\5\1\264\17\5\1\264\5\5"+
    "\22\0\2\5\3\0\10\5\1\265\20\5\1\265\11\5"+
    "\22\0\2\5\3\0\6\5\1\266\24\5\1\266\7\5"+
    "\20\0";

  private static int [] zzUnpackTrans() {
    int [] result = new int[7366];
    int offset = 0;
    offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackTrans(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      value--;
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /* error codes */
  private static final int ZZ_UNKNOWN_ERROR = 0;
  private static final int ZZ_NO_MATCH = 1;
  private static final int ZZ_PUSHBACK_2BIG = 2;

  /* error messages for the codes above */
  private static final String ZZ_ERROR_MSG[] = {
    "Unkown internal scanner error",
    "Error: could not match input",
    "Error: pushback value was too large"
  };

  /**
   * ZZ_ATTRIBUTE[aState] contains the attributes of state <code>aState</code>
   */
  private static final int [] ZZ_ATTRIBUTE = zzUnpackAttribute();

  private static final String ZZ_ATTRIBUTE_PACKED_0 =
    "\1\1\1\11\3\1\1\11\31\1\5\11\1\1\3\11"+
    "\2\1\3\11\43\1\3\11\2\0\1\11\40\1\1\11"+
    "\77\1";

  private static int [] zzUnpackAttribute() {
    int [] result = new int[182];
    int offset = 0;
    offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAttribute(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }

  /** the input device */
  private java.io.Reader zzReader;

  /** the current state of the DFA */
  private int zzState;

  /** the current lexical state */
  private int zzLexicalState = YYINITIAL;

  /** this buffer contains the current text to be matched and is
      the source of the yytext() string */
  private char zzBuffer[] = new char[ZZ_BUFFERSIZE];

  /** the textposition at the last accepting state */
  private int zzMarkedPos;

  /** the current text position in the buffer */
  private int zzCurrentPos;

  /** startRead marks the beginning of the yytext() string in the buffer */
  private int zzStartRead;

  /** endRead marks the last character in the buffer, that has been read
      from input */
  private int zzEndRead;

  /** number of newlines encountered up to the start of the matched text */
  private int yyline;

  /** the number of characters up to the start of the matched text */
  private int yychar;

  /**
   * the number of characters from the last newline up to the start of the 
   * matched text
   */
  private int yycolumn;

  /** 
   * zzAtBOL == true <=> the scanner is currently at the beginning of a line
   */
  private boolean zzAtBOL = true;

  /** zzAtEOF == true <=> the scanner is at the EOF */
  private boolean zzAtEOF;

  /** denotes if the user-EOF-code has already been executed */
  private boolean zzEOFDone;

  /* user code: */
// hier kann beliebiger Code stehen, der einkopiert wird

public static final int whitespace 	= 256;
public static final int ident 		= 257;
public static final int intconst	= 258;
public static final int lpar		= 259;
public static final int rpar		= 260;
public static final int addop		= 261;
public static final int subop		= 262;
public static final int multop		= 263;
public static final int divop		= 264;
public static final int ifsy		= 265;
public static final int thensy		= 266;
public static final int elsesy		= 267;
public static final int beginsy		= 268;
public static final int endsy		= 269;
public static final int whilesy		= 270;
public static final int dosy		= 271;
public static final int becomesy	= 272;
public static final int semicolon	= 273;
public static final int eqsy		= 274;
public static final int neqsy		= 275;
public static final int ltsy		= 276;
public static final int gtsy		= 277;
public static final int lesy		= 278;
public static final int gesy		= 279;

public static final int modulesy	= 301;
public static final int constsy		= 280;
public static final int varsy		= 281;
public static final int typesy		= 282;
public static final int comma		= 283;
public static final int colon		= 284;

public static final int loopsy		= 285;
public static final int exitsy		= 286;
public static final int period		= 287;

public static final int repeatsy	= 288;
public static final int untilsy		= 289;

public static final int arraysy		= 290;
public static final int recordsy	= 291;

public static final int integersy	= 292;
public static final int realsy		= 293;
public static final int charsy		= 294;
public static final int booleansy	= 295;
public static final int stringsy	= 296;

public static final int divsy		= 297;
public static final int modsy		= 298;
public static final int orsy		= 299;
public static final int proceduresy	= 300;

public static final int lbrack	= 302;
public static final int rbrack	= 303;

public static final int realconst	= 304;
public static final int charconst	= 305;
public static final int stringconst	= 306;
public static final int truesy		= 307;
public static final int falsesy		= 308;
public static final int ofsy		= 309;
public static final int printsy		= 310;

public static final int forwardsy	= 311;
public static final int returnsy	= 312;
public static final int functionsy	= 313;

public static final int andsy		= 314;
public static final int notsy		= 315;
public static final int elsifsy		= 316;
public static final int readsy		= 317;
public static final int errorsy		= 255;

public static int intval; 
public static String strval;
public static char cval;
public static float rval;
public static boolean bval;

public static int line=1, column=1;
void pos(){line = this.yyline; column = this.yycolumn;}



  /**
   * Creates a new scanner
   * There is also a java.io.InputStream version of this constructor.
   *
   * @param   in  the java.io.Reader to read input from.
   */
  public Oberon0Lexer(java.io.Reader in) {
    this.zzReader = in;
  }

  /**
   * Creates a new scanner.
   * There is also java.io.Reader version of this constructor.
   *
   * @param   in  the java.io.Inputstream to read input from.
   */
  public Oberon0Lexer(java.io.InputStream in) {
    this(new java.io.InputStreamReader(in));
  }

  /** 
   * Unpacks the compressed character translation table.
   *
   * @param packed   the packed character translation table
   * @return         the unpacked character translation table
   */
  private static char [] zzUnpackCMap(String packed) {
    char [] map = new char[0x10000];
    int i = 0;  /* index in packed string  */
    int j = 0;  /* index in unpacked array */
    while (i < 164) {
      int  count = packed.charAt(i++);
      char value = packed.charAt(i++);
      do map[j++] = value; while (--count > 0);
    }
    return map;
  }


  /**
   * Refills the input buffer.
   *
   * @return      <code>false</code>, iff there was new input.
   * 
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  private boolean zzRefill() throws java.io.IOException {

    /* first: make room (if you can) */
    if (zzStartRead > 0) {
      System.arraycopy(zzBuffer, zzStartRead,
                       zzBuffer, 0,
                       zzEndRead-zzStartRead);

      /* translate stored positions */
      zzEndRead-= zzStartRead;
      zzCurrentPos-= zzStartRead;
      zzMarkedPos-= zzStartRead;
      zzStartRead = 0;
    }

    /* is the buffer big enough? */
    if (zzCurrentPos >= zzBuffer.length) {
      /* if not: blow it up */
      char newBuffer[] = new char[zzCurrentPos*2];
      System.arraycopy(zzBuffer, 0, newBuffer, 0, zzBuffer.length);
      zzBuffer = newBuffer;
    }

    /* finally: fill the buffer with new input */
    int numRead = zzReader.read(zzBuffer, zzEndRead,
                                            zzBuffer.length-zzEndRead);

    if (numRead > 0) {
      zzEndRead+= numRead;
      return false;
    }
    // unlikely but not impossible: read 0 characters, but not at end of stream    
    if (numRead == 0) {
      int c = zzReader.read();
      if (c == -1) {
        return true;
      } else {
        zzBuffer[zzEndRead++] = (char) c;
        return false;
      }     
    }

	// numRead < 0
    return true;
  }

    
  /**
   * Closes the input stream.
   */
  public final void yyclose() throws java.io.IOException {
    zzAtEOF = true;            /* indicate end of file */
    zzEndRead = zzStartRead;  /* invalidate buffer    */

    if (zzReader != null)
      zzReader.close();
  }


  /**
   * Resets the scanner to read from a new input stream.
   * Does not close the old reader.
   *
   * All internal variables are reset, the old input stream 
   * <b>cannot</b> be reused (internal buffer is discarded and lost).
   * Lexical state is set to <tt>ZZ_INITIAL</tt>.
   *
   * @param reader   the new input stream 
   */
  public final void yyreset(java.io.Reader reader) {
    zzReader = reader;
    zzAtBOL  = true;
    zzAtEOF  = false;
    zzEOFDone = false;
    zzEndRead = zzStartRead = 0;
    zzCurrentPos = zzMarkedPos = 0;
    yyline = yychar = yycolumn = 0;
    zzLexicalState = YYINITIAL;
  }


  /**
   * Returns the current lexical state.
   */
  public final int yystate() {
    return zzLexicalState;
  }


  /**
   * Enters a new lexical state
   *
   * @param newState the new lexical state
   */
  public final void yybegin(int newState) {
    zzLexicalState = newState;
  }


  /**
   * Returns the text matched by the current regular expression.
   */
  public final String yytext() {
    return new String( zzBuffer, zzStartRead, zzMarkedPos-zzStartRead );
  }


  /**
   * Returns the character at position <tt>pos</tt> from the 
   * matched text. 
   * 
   * It is equivalent to yytext().charAt(pos), but faster
   *
   * @param pos the position of the character to fetch. 
   *            A value from 0 to yylength()-1.
   *
   * @return the character at position pos
   */
  public final char yycharat(int pos) {
    return zzBuffer[zzStartRead+pos];
  }


  /**
   * Returns the length of the matched text region.
   */
  public final int yylength() {
    return zzMarkedPos-zzStartRead;
  }


  /**
   * Reports an error that occured while scanning.
   *
   * In a wellformed scanner (no or only correct usage of 
   * yypushback(int) and a match-all fallback rule) this method 
   * will only be called with things that "Can't Possibly Happen".
   * If this method is called, something is seriously wrong
   * (e.g. a JFlex bug producing a faulty scanner etc.).
   *
   * Usual syntax/scanner level error handling should be done
   * in error fallback rules.
   *
   * @param   errorCode  the code of the errormessage to display
   */
  private void zzScanError(int errorCode) {
    String message;
    try {
      message = ZZ_ERROR_MSG[errorCode];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
    }

    throw new Error(message);
  } 


  /**
   * Pushes the specified amount of characters back into the input stream.
   *
   * They will be read again by then next call of the scanning method
   *
   * @param number  the number of characters to be read again.
   *                This number must not be greater than yylength()!
   */
  public void yypushback(int number)  {
    if ( number > yylength() )
      zzScanError(ZZ_PUSHBACK_2BIG);

    zzMarkedPos -= number;
  }


  /**
   * Contains user EOF-code, which will be executed exactly once,
   * when the end of file is reached
   */
  private void zzDoEOF() throws java.io.IOException {
    if (!zzEOFDone) {
      zzEOFDone = true;
      yyclose();
    }
  }


  /**
   * Resumes scanning until the next regular expression is matched,
   * the end of input is encountered or an I/O-Error occurs.
   *
   * @return      the next token
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  public int yylex() throws java.io.IOException {
    int zzInput;
    int zzAction;

    // cached fields:
    int zzCurrentPosL;
    int zzMarkedPosL;
    int zzEndReadL = zzEndRead;
    char [] zzBufferL = zzBuffer;
    char [] zzCMapL = ZZ_CMAP;

    int [] zzTransL = ZZ_TRANS;
    int [] zzRowMapL = ZZ_ROWMAP;
    int [] zzAttrL = ZZ_ATTRIBUTE;

    while (true) {
      zzMarkedPosL = zzMarkedPos;

      boolean zzR = false;
      for (zzCurrentPosL = zzStartRead; zzCurrentPosL < zzMarkedPosL;
                                                             zzCurrentPosL++) {
        switch (zzBufferL[zzCurrentPosL]) {
        case '\u000B':
        case '\u000C':
        case '\u0085':
        case '\u2028':
        case '\u2029':
          yyline++;
          yycolumn = 0;
          zzR = false;
          break;
        case '\r':
          yyline++;
          yycolumn = 0;
          zzR = true;
          break;
        case '\n':
          if (zzR)
            zzR = false;
          else {
            yyline++;
            yycolumn = 0;
          }
          break;
        default:
          zzR = false;
          yycolumn++;
        }
      }

      if (zzR) {
        // peek one character ahead if it is \n (if we have counted one line too much)
        boolean zzPeek;
        if (zzMarkedPosL < zzEndReadL)
          zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        else if (zzAtEOF)
          zzPeek = false;
        else {
          boolean eof = zzRefill();
          zzEndReadL = zzEndRead;
          zzMarkedPosL = zzMarkedPos;
          zzBufferL = zzBuffer;
          if (eof) 
            zzPeek = false;
          else 
            zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        }
        if (zzPeek) yyline--;
      }
      zzAction = -1;

      zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;
  
      zzState = ZZ_LEXSTATE[zzLexicalState];


      zzForAction: {
        while (true) {
    
          if (zzCurrentPosL < zzEndReadL)
            zzInput = zzBufferL[zzCurrentPosL++];
          else if (zzAtEOF) {
            zzInput = YYEOF;
            break zzForAction;
          }
          else {
            // store back cached positions
            zzCurrentPos  = zzCurrentPosL;
            zzMarkedPos   = zzMarkedPosL;
            boolean eof = zzRefill();
            // get translated positions and possibly new buffer
            zzCurrentPosL  = zzCurrentPos;
            zzMarkedPosL   = zzMarkedPos;
            zzBufferL      = zzBuffer;
            zzEndReadL     = zzEndRead;
            if (eof) {
              zzInput = YYEOF;
              break zzForAction;
            }
            else {
              zzInput = zzBufferL[zzCurrentPosL++];
            }
          }
          int zzNext = zzTransL[ zzRowMapL[zzState] + zzCMapL[zzInput] ];
          if (zzNext == -1) break zzForAction;
          zzState = zzNext;

          int zzAttributes = zzAttrL[zzState];
          if ( (zzAttributes & 1) == 1 ) {
            zzAction = zzState;
            zzMarkedPosL = zzCurrentPosL;
            if ( (zzAttributes & 8) == 8 ) break zzForAction;
          }

        }
      }

      // store back cached position
      zzMarkedPos = zzMarkedPosL;

      switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {
        case 39: 
          { pos(); return exitsy;
          }
        case 64: break;
        case 22: 
          { pos(); return orsy;
          }
        case 65: break;
        case 17: 
          { pos(); return gtsy;
          }
        case 66: break;
        case 10: 
          { pos(); return multop;
          }
        case 67: break;
        case 37: 
          { pos(); return loopsy;
          }
        case 68: break;
        case 1: 
          { pos(); return whitespace;
          }
        case 69: break;
        case 47: 
          { pos(); return elsifsy;
          }
        case 70: break;
        case 26: 
          { pos(); return becomesy;
          }
        case 71: break;
        case 53: 
          { pos(); return whilesy;
          }
        case 72: break;
        case 61: 
          { strval = new String(yytext());pos(); return booleansy;
          }
        case 73: break;
        case 60: 
          { strval = new String(yytext());pos(); return integersy;
          }
        case 74: break;
        case 55: 
          { pos(); return recordsy;
          }
        case 75: break;
        case 23: 
          { pos(); return ofsy;
          }
        case 76: break;
        case 6: 
          { pos(); return divop;
          }
        case 77: break;
        case 29: 
          { strval = new String(yytext().substring(1, yytext().length()-1));
				 pos(); return stringconst;
          }
        case 78: break;
        case 57: 
          { pos(); return repeatsy;
          }
        case 79: break;
        case 62: 
          { pos(); return functionsy;
          }
        case 80: break;
        case 49: 
          { pos(); return arraysy;
          }
        case 81: break;
        case 46: 
          { pos(); return untilsy;
          }
        case 82: break;
        case 30: 
          { pos(); return modsy;
          }
        case 83: break;
        case 56: 
          { pos(); return returnsy;
          }
        case 84: break;
        case 13: 
          { pos(); return eqsy;
          }
        case 85: break;
        case 11: 
          { pos(); return subop;
          }
        case 86: break;
        case 59: 
          { pos(); return forwardsy;
          }
        case 87: break;
        case 48: 
          { pos(); return constsy;
          }
        case 88: break;
        case 45: 
          { strval = new String(yytext());pos(); return charsy;
          }
        case 89: break;
        case 7: 
          { pos(); return lpar;
          }
        case 90: break;
        case 18: 
          { pos(); return comma;
          }
        case 91: break;
        case 42: 
          { pos(); return thensy;
          }
        case 92: break;
        case 35: 
          { pos(); return andsy;
          }
        case 93: break;
        case 33: 
          { pos(); return notsy;
          }
        case 94: break;
        case 44: 
          { strval = new String(yytext());pos(); return realsy;
          }
        case 95: break;
        case 58: 
          { strval = new String(yytext());pos(); return stringsy;
          }
        case 96: break;
        case 41: 
          { pos(); return typesy;
          }
        case 97: break;
        case 32: 
          { pos(); return endsy;
          }
        case 98: break;
        case 40: 
          { bval = true; pos(); return truesy;
          }
        case 99: break;
        case 36: 
          { cval = yytext().charAt(1);
				 pos(); return charconst;
          }
        case 100: break;
        case 12: 
          { pos(); return colon;
          }
        case 101: break;
        case 27: 
          { pos(); return lesy;
          }
        case 102: break;
        case 63: 
          { pos(); return proceduresy;
          }
        case 103: break;
        case 3: 
          { intval = Integer.parseInt(yytext());
			 pos(); return intconst;
          }
        case 104: break;
        case 5: 
          { pos(); return period;
          }
        case 105: break;
        case 9: 
          { pos(); return addop;
          }
        case 106: break;
        case 34: 
          { pos(); return varsy;
          }
        case 107: break;
        case 14: 
          { pos(); return semicolon;
          }
        case 108: break;
        case 16: 
          { pos(); return ltsy;
          }
        case 109: break;
        case 50: 
          { strval = new String(yytext());pos(); return printsy;
          }
        case 110: break;
        case 31: 
          { pos(); return divsy;
          }
        case 111: break;
        case 25: 
          { pos(); return ifsy;
          }
        case 112: break;
        case 8: 
          { pos(); return rpar;
          }
        case 113: break;
        case 19: 
          { pos(); return lbrack;
          }
        case 114: break;
        case 24: 
          { pos(); return dosy;
          }
        case 115: break;
        case 20: 
          { pos(); return rbrack;
          }
        case 116: break;
        case 43: 
          { pos(); return readsy;
          }
        case 117: break;
        case 21: 
          { rval = Float.parseFloat(yytext());
			 pos(); return realconst;
          }
        case 118: break;
        case 4: 
          { strval = new String(yytext());
				 pos(); return ident;
          }
        case 119: break;
        case 52: 
          { bval = false; pos(); return falsesy;
          }
        case 120: break;
        case 38: 
          { pos(); return elsesy;
          }
        case 121: break;
        case 2: 
          { pos(); return errorsy;
          }
        case 122: break;
        case 28: 
          { pos(); return gesy;
          }
        case 123: break;
        case 54: 
          { pos(); return modulesy;
          }
        case 124: break;
        case 15: 
          { pos(); return neqsy;
          }
        case 125: break;
        case 51: 
          { pos(); return beginsy;
          }
        case 126: break;
        default: 
          if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {
            zzAtEOF = true;
            zzDoEOF();
              { return 0; }
          } 
          else {
            zzScanError(ZZ_NO_MATCH);
          }
      }
    }
  }


}
