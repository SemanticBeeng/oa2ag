import java.io.*;
import java.util.*;

public class Czero {

  static String tFile;

  static int tIntValue;
  static String tIdValue;
  static int tKind;

  static FileReader in;
  static int c;

  static FileWriter out;
  
  static void code(String s) {
    try {
      out.write(s+"\n");
    } catch(Exception e) { e.printStackTrace(); }
  }

  static void parseError() {
    System.out.print("*** "+tFile+":"+tLine+":"+tCol+": unexpected token: ");
    printToken();
    System.out.println();
    System.exit(1);
  }

  static void compileError(String s) {
    System.out.println("*** "+tFile+":"+tLine+":"+tCol+": "+s);
    System.exit(1);
  }  

  static int nextLabel = 0;

  static void parseProgram() {
    Map funcs = new HashMap();
    Map prototypes = new HashMap();
    skipToken(tSHARP);
    checkToken(tINCLUDE);
    skipToken(tLT);
    checkToken("stdio");
    checkToken(tDOT);
    checkToken("h");
    checkToken(tGT);
    parseFunctions(funcs,prototypes);
    parseMain(funcs,prototypes);
    Iterator i = prototypes.keySet().iterator();
    while (i.hasNext()) {
      String s = (String)i.next();
      if (!funcs.containsKey(s))
        compileError("missing implementation of "+s);
    }
  }

  static void parseMain(Map funcs, Map prototypes) {
    Map vars = new HashMap();
    skipToken(tMAIN);
    skipToken(tLPAR);
    skipToken(tRPAR);
    code(".method main");
    code(".args 1");
    parseBody(0,vars,funcs,prototypes);
    code("bipush 0");
    code("ireturn");
    skipToken(tEOF);
  }

  static void parseFunctions(Map funcs, Map prototypes) {
    skip();
    while (tKind==tINT) {
      parseFunction(funcs,prototypes);
      skip();
    }
  }

  static void parseFunction(Map funcs, Map prototypes) {
    Map vars = new HashMap();
    String name;
    checkToken(tINT);
    skipToken(tID);
    name = tIdValue;
    skipToken(tLPAR);
    int args = parseFormals(vars);
    skipToken(tRPAR);
    skip();
    if (tKind==tSEMI) {
      nextToken();
      if (prototypes.containsKey(name)) 
        compileError("duplicate declaration of "+name);
      if (funcs.containsKey(name) && args!=funcs.get(name))
        compileError("conflicting declaration of "+name);
      prototypes.put(name,args);
    } else {
      if (funcs.containsKey(name))
        compileError("duplicate implementation of "+name);
      if (prototypes.containsKey(name) && args!=prototypes.get(name))
        compileError("conflicting implementation of "+name);
      funcs.put(name,args);
      code(".method "+name);
      code(".args "+(args+1));
      parseBody(args,vars,funcs,prototypes);
      code("bipush 0");
      code("ireturn");
    }
  }

  static int parseFormals(Map vars) {
    int offset = 0;
    boolean more = false;
    skip();
    while (tKind==tINT) {
      nextToken();
      skipToken(tID);
      offset++;
      vars.put(tIdValue,offset);
      skip();
      if (tKind==tCOMMA) {
        nextToken();
        skip();
        more = true;
      } else
        more = false;
    }
    if (more) parseError();
    return offset;
  }

  static void parseBody(int offset, Map vars, Map funcs, Map prototypes) {
    skipToken(tLBRACK);
    parseDeclarations(offset,0,vars);
    parseStatements(vars,funcs,prototypes); 
    skipToken(tRBRACK);
  }

  static void parseDeclarations(int offset, int locals, Map vars) {
    int initValue = 0;
    boolean initialized = false;
    skip();
    if (tKind==tINT) {
      nextToken();
      skipToken(tID);
      offset++;
      vars.put(tIdValue,offset);
      skip();
      if (tKind==tASSIGN) {
        nextToken();
        skipToken(tCONST);
        initialized = true;
        initValue = tIntValue;
      }
      checkToken(tSEMI);
      parseDeclarations(offset,locals+1,vars);
    } else {
      if (locals>0)
        code(".locals "+locals);
    }
    if (initialized) {
      code("bipush "+initValue);
      code("istore "+offset);
    }
  }

  static void parseStatements(Map vars, Map funcs, Map prototypes) {
    skip();
    while (tKind!=tRBRACK) {
      parseStatement(vars,funcs,prototypes);
      skip();
    }
  }

  static void parseStatement(Map vars, Map funcs, Map prototypes) {
    if (tKind==tID) {
      String name = tIdValue;
      nextToken();
      skipToken(tASSIGN);
      if (!vars.containsKey(name))
        compileError("undeclared variable: "+name);
      skip();
      parseExpression(vars,funcs,prototypes);
      skipToken(tSEMI);
      code("istore "+vars.get(name));
    } else if (tKind==tRETURN) {
      nextToken();
      skip();
      parseExpression(vars,funcs,prototypes);
      skipToken(tSEMI);
      code("ireturn");
    } else if (tKind==tIF) {
      int elselabel = nextLabel++;
      int donelabel = nextLabel++;
      nextToken();
      skipToken(tLPAR);
      skip();
      parseExpression(vars,funcs,prototypes);
      skipToken(tRPAR);
      code("ifeq L"+elselabel);
      skipToken(tLBRACK);
      parseStatements(vars,funcs,prototypes);
      skipToken(tRBRACK);
      code("goto L"+donelabel);
      code("L"+elselabel+":");
      skip();
      if (tKind==tELSE) {
        nextToken();
        skipToken(tLBRACK);
        skip();
        parseStatements(vars,funcs,prototypes);
        skipToken(tRBRACK);
      }
      code("L"+donelabel+":");
    } else if (tKind==tWHILE) {
      int looplabel = nextLabel++;
      int donelabel = nextLabel++;
      code("L"+looplabel+":");
      nextToken();
      skipToken(tLPAR);
      skip();
      parseExpression(vars,funcs,prototypes);
      skipToken(tRPAR);
      code("ifeq L"+donelabel);
      skipToken(tLBRACK);
      skip();
      parseStatements(vars,funcs,prototypes); 
      skipToken(tRBRACK);
      code("goto L"+looplabel);
      code("L"+donelabel+":");
    } else if (tKind==tPUTCHAR) {
      nextToken();
      skipToken(tLPAR);
      code("bipush 44");
      skip();
      parseExpression(vars,funcs,prototypes);
      code("invokevirtual putchar");
      skipToken(tRPAR);
      skipToken(tSEMI);
    } else parseError();
  }

  static void parseExpression(Map vars, Map funcs, Map prototypes) {
    parseExp3(vars,funcs,prototypes);
  }

  static void parseExp0(Map vars, Map funcs, Map prototypes) {
    if (tKind==tCONST) {
      code("ldc_w "+tIntValue);
      nextToken();
    } else if (tKind==tGETCHAR) {
      nextToken();
      skipToken(tLPAR);
      skipToken(tRPAR);
      code("bipush 44");
      code("invokevirtual getchar");
    } else if (tKind==tLPAR) {
      nextToken();
      parseExpression(vars,funcs,prototypes);
      skipToken(tRPAR);
    } else if (tKind==tID) {
      String name = tIdValue;
      nextToken();
      skip();
      if (tKind==tLPAR) {
        nextToken();
        skip();
        code("bipush 44");
        int args = parseActuals(vars,funcs,prototypes);
        code("invokevirtual "+name);
        skipToken(tRPAR);
        if (funcs.containsKey(name)) {
          if (args!=funcs.get(name))
            compileError("incorrect number of arguments: "+name);
        } else if (prototypes.containsKey(name)) {
          if (args!=prototypes.get(name))
            compileError("incorrect number of arguments: "+name);
        } else {
          compileError("undeclared function: "+name);
        }
      } else {
        if (!vars.containsKey(name))
          compileError("undeclared variable: "+name);
        code("iload "+vars.get(name));
      }
    } else if (tKind==tSUB) {
      nextToken();
      skip();
      code("bipush 0");
      parseExp0(vars,funcs,prototypes);
      code("isub");
    } else if (tKind==tNOT) {
      nextToken();
      skip();
      code("bipush 44");
      parseExp0(vars,funcs,prototypes);
      code("invokevirtual not_");
    } else parseError();
  }

  static void parseExp1(Map vars, Map funcs, Map prototypes) {
    parseExp0(vars,funcs,prototypes);
    skip();
    while (tKind==tMUL || tKind==tDIV || tKind==tAND || tKind==tMOD) {
      int op = tKind;
      nextToken();
      skip();
      if (op==tMUL || op==tDIV || op==tMOD) {
        code("bipush 44");
        code("swap");
      }
      parseExp0(vars,funcs,prototypes);
      switch (op) {
        case tMUL: code("invokevirtual mul_"); break;
        case tDIV: code("invokevirtual div_"); break;
        case tAND: code("iand"); break;
        case tMOD: code("invokevirtual mod_"); break;
      }
    }
  }

  static void parseExp2(Map vars, Map funcs, Map prototypes) {
    parseExp1(vars,funcs,prototypes);
    skip();
    while (tKind==tADD || tKind==tSUB || tKind==tOR) {
      int op = tKind;
      nextToken();
      skip();
      parseExp1(vars,funcs,prototypes);
      switch (op) {
        case tADD: code("iadd"); break;
        case tSUB: code("isub"); break;
        case tOR:  code("ior"); break;
      }
    }
  }

  static void parseExp3(Map vars, Map funcs, Map prototypes) {
    parseExp2(vars,funcs,prototypes);
    skip();
    while (tKind==tEQ || tKind==tNE || tKind==tLT || tKind==tGT || tKind==tLEQ || tKind==tGEQ) {
      int op = tKind;
      nextToken();
      skip();
      code("bipush 44");
      code("swap");
      parseExp2(vars,funcs,prototypes);
      switch (op) {
        case tEQ:  code("invokevirtual eq_"); break;
        case tNE:  code("invokevirtual ne_"); break;
        case tLT:  code("invokevirtual lt_"); break;
        case tGT:  code("invokevirtual gt_"); break;
        case tLEQ: code("invokevirtual leq_"); break;
        case tGEQ: code("invokevirtual geq_"); break;
      }
    }
  }

  static int parseActuals(Map vars, Map funcs, Map prototypes) {
    boolean more = false;
    int args = 0;
    skip();
    while (tKind!=tRPAR) {
      parseExpression(vars,funcs,prototypes);
      args++;
      skip();
      if (tKind==tCOMMA) {
        nextToken();
        skip();
        more = true;
      } else
        more = false;
    }
    if (more) parseError();
    return args;
  }
}
