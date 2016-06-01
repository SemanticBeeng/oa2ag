  static int tLine;
  static int tCol;

  static final int tLPAR = 0;
  static final int tRPAR = 1;
  static final int tASSIGN = 2;
  static final int tSEMI = 3;
  static final int tCOMMA = 4;
  static final int tEQ = 5;
  static final int tNE = 6;
  static final int tID = 7;
  static final int tCONST = 8;
  static final int tCHAR = 9;
  static final int tADD = 10;
  static final int tSUB = 11;
  static final int tMUL = 12;
  static final int tDIV = 13;
  static final int tMOD = 14;
  static final int tLBRACK = 15;
  static final int tRBRACK = 16;
  static final int tOR = 17;
  static final int tAND = 18;
  static final int tNOT = 19;
  static final int tLT = 20;
  static final int tGT = 21;
  static final int tLEQ = 22;
  static final int tGEQ = 23;
  static final int tSHARP = 24;
  static final int tDOT = 25;
  static final int tINT = 26;
  static final int tIF = 27;
  static final int tELSE = 28;
  static final int tWHILE = 29;
  static final int tGETCHAR = 30;
  static final int tPUTCHAR = 31;
  static final int tINCLUDE = 32;
  static final int tRETURN = 33;
  static final int tMAIN = 34;
  static final int tEOF = 35;
  static final int tERROR = 36;
  static final int tWHITE = 37;

  static int nextChar() {
    try {
      c = in.read();
    } catch (Exception e) {
      c = -1;
    }
    if (c=='\n') {
      tLine++;
      tCol = 1;
    } else tCol++;
    return c;
  }


  static boolean letter(int c) {
    return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
  }

  static boolean digit(int c) {
    return ('0' <= c && c <= '9');
  }

  static boolean alpha(int c) {
    return letter(c) || digit(c);
  }
  static int nextToken() {
    switch (c) {
      case '(': tKind = tLPAR;   nextChar(); break;
      case ')': tKind = tRPAR;   nextChar(); break;
      case ';': tKind = tSEMI;   nextChar(); break;
      case ',': tKind = tCOMMA;  nextChar(); break;
      case '+': tKind = tADD;    nextChar(); break;
      case '-': tKind = tSUB;    nextChar(); break;
      case '*': tKind = tMUL;    nextChar(); break;
      case '/': tKind = tDIV;    nextChar(); break;
      case '%': tKind = tMOD;    nextChar(); break;
      case '{': tKind = tLBRACK; nextChar(); break;
      case '}': tKind = tRBRACK; nextChar(); break;
      case '|': tKind = tOR;     nextChar(); break;
      case '&': tKind = tAND;    nextChar(); break;
      case '#': tKind = tSHARP;  nextChar(); break;
      case '.': tKind = tDOT;    nextChar(); break;
      case '\n':
      case ' ': tKind = tWHITE;  nextChar(); break;
      case '\'': nextChar();
                if (c=='\\') {
                  nextChar();
                  if (c!='n') {
                    tKind = tERROR; 
                    break; 
                  }
                  nextChar();
                  if (c!='\'') {
                    tKind = tERROR; 
                    break; 
                  }
                  nextChar();
                  tKind = tCONST;
                  tIntValue = 10;
                  break;
                } 
                if (c==-1 || c=='\'') { 
                  tKind = tERROR; 
                  break; 
                }
                tKind = tCONST;
                tIntValue = c;
                nextChar();
                if (c!='\'') {
                  tKind = tERROR;
                  break;
                }
                nextChar();
                break;
      case '=': nextChar(); 
                if (c=='=') { 
                  nextChar();
                  tKind = tEQ; 
                } else {
                  tKind = tASSIGN;
                }
                break;
      case '<': nextChar();
                if (c=='=') {
                  nextChar();
                  tKind = tLEQ;
                } else {
                  tKind = tLT;
                }
                break;
      case '>': nextChar();
                if (c=='=') {
                  nextChar();
                  tKind = tGEQ;
                } else {
                  tKind = tGT;
                }
                break;
      case '!': nextChar();
                if (c=='=') {
                  nextChar();
                  tKind = tNE;
                } else {
                  tKind = tNOT;
                }
                break;
      case -1: tKind = tEOF; break;
      default: if (letter(c)) {
                 tKind = tID;
                 tIdValue = "";
                 while (alpha(c)) {
                   tIdValue = tIdValue+(char)c;
                   nextChar();
                 }
                 if (tIdValue.equals("int")) tKind = tINT;
                 else if (tIdValue.equals("if")) tKind = tIF;
                 else if (tIdValue.equals("else")) tKind = tELSE;
                 else if (tIdValue.equals("while")) tKind = tWHILE;
                 else if (tIdValue.equals("getchar")) tKind = tGETCHAR;
                 else if (tIdValue.equals("putchar")) tKind = tPUTCHAR;
                 else if (tIdValue.equals("include")) tKind = tINCLUDE;
                 else if (tIdValue.equals("return")) tKind = tRETURN;
                 else if (tIdValue.equals("main")) tKind = tMAIN;
               } else if (digit(c)) {
                 tKind = tCONST;
                 tIntValue = 0;
                 while (digit(c)) {
                   tIntValue = 10*tIntValue+c-'0';
                   nextChar();
                 }
               } else {
                 tKind = tERROR;
               }
    }
    return tKind;
  }

  static void skip() {
    while (tKind==tWHITE) nextToken(); 
  }

  static void checkToken(int k) {
    if (tKind!=k) parseError();
    nextToken();
  }

  static void skipToken(int k) {
    skip();
    checkToken(k);
  }

  static void checkToken(String s) {
    if (tKind!=tID || !tIdValue.equals(s)) parseError();
    nextToken();
  }


static void printToken() {
    switch (tKind) {
      case tLPAR: System.out.print("("); break;
      case tRPAR: System.out.print(")"); break;
      case tASSIGN: System.out.print("="); break;
      case tSEMI: System.out.print(";"); break;
      case tCOMMA: System.out.print(","); break;
      case tEQ: System.out.print("=="); break;
      case tNE: System.out.print("!="); break;
      case tID: System.out.print(tIdValue); break;
      case tCONST: System.out.print(tIntValue); break;
      case tCHAR: System.out.print("char"); break;
      case tADD: System.out.print("+"); break;
      case tSUB: System.out.print("-"); break;
      case tMUL: System.out.print("*"); break;
      case tDIV: System.out.print("/"); break;
      case tMOD: System.out.print("%"); break;
      case tLBRACK: System.out.print("{"); break;
      case tRBRACK: System.out.print("}"); break;
      case tOR: System.out.print("|"); break;
      case tAND: System.out.print("&"); break;
      case tNOT: System.out.print("!"); break;
      case tLT: System.out.print("<"); break;
      case tGT: System.out.print(">"); break;
      case tLEQ: System.out.print("<="); break;
      case tGEQ: System.out.print(">="); break;
      case tSHARP: System.out.print("#"); break;
      case tDOT: System.out.print("."); break;
      case tINT: System.out.print("int"); break;
      case tIF: System.out.print("if"); break;
      case tELSE: System.out.print("else"); break;
      case tWHILE: System.out.print("while"); break;
      case tGETCHAR: System.out.print("getchar"); break;
      case tPUTCHAR: System.out.print("putchar"); break;
      case tINCLUDE: System.out.print("include"); break;
      case tRETURN: System.out.print("return"); break;
      case tMAIN: System.out.print("main"); break;
      case tEOF: System.out.print("eof"); break;
      case tERROR: System.out.print("error"); break;
      case tWHITE: System.out.print(" "); break;
    }
  }
