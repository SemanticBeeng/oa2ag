  static void codeLibrary() {
    code(".method mul_");
    code(".args 3");
    code(".locals 2");
    code("bipush 1");
    code("istore 4");
    code("bipush 0");
    code("iload 1");
    code("isub");
    code("iflt mul0");
    code("bipush 0");
    code("iload 1 ");
    code("isub");
    code("istore 1");
    code("bipush 0");
    code("istore 4");
    code("mul0:");
    code("bipush 0");
    code("istore 3");
    code("mul1:");
    code("iload 1");
    code("ifeq mul2");
    code("iload 1");
    code("bipush 1");
    code("isub");
    code("istore 1");
    code("iload 3");
    code("iload 2");
    code("iadd");
    code("istore 3");
    code("goto mul1");
    code("mul2:");
    code("bipush 1");
    code("iload 4");
    code("isub ");
    code("ifeq mul3");
    code("bipush 0");
    code("iload 3");
    code("isub");
    code("istore 3");
    code("mul3:");
    code("iload 3");
    code("ireturn");
    code("");
    code(".method div_");
    code(".args 3");
    code(".locals 2");
    code("bipush 1");
    code("istore 4");
    code("bipush 0");
    code("iload 1");
    code("isub");
    code("iflt div0");
    code("bipush 0");
    code("iload 1 ");
    code("isub");
    code("istore 1");
    code("bipush 0");
    code("istore 4");
    code("div0:");
    code("bipush 0");
    code("iload 2");
    code("isub");
    code("iflt div1");
    code("bipush 0");
    code("iload 2 ");
    code("isub");
    code("istore 2");
    code("iload 4");
    code("bipush 1");
    code("iadd");
    code("istore 4");
    code("div1:");
    code("bipush 0");
    code("istore 3");
    code("div2:");
    code("iload 1");
    code("iload 2");
    code("isub");
    code("iflt div3");
    code("iload 1");
    code("iload 2");
    code("isub");
    code("istore 1");
    code("iload 3");
    code("bipush 1");
    code("iadd");
    code("istore 3");
    code("goto div2");
    code("div3:");
    code("bipush 0");
    code("iload 4");
    code("isub ");
    code("iflt div4");
    code("bipush 0");
    code("iload 3");
    code("isub");
    code("istore 3");
    code("div4:");
    code("iload 3"); 
    code("ireturn"); 
    code("");
    code(".method mod_");
    code(".args 3");
    code(".locals 1");
    code("bipush 44");
    code("iload 1");
    code("iload 2");
    code("invokevirtual div_");
    code("istore 3");
    code("iload 1");
    code("bipush 44");
    code("iload 3");
    code("iload 2");
    code("invokevirtual mul_");
    code("isub");
    code("ireturn"); 
    code("");
    code(".method not_");
    code(".args 2");
    code("iload 1");
    code("ifeq not1");
    code("bipush 0");
    code("goto not0");
    code("not1:");
    code("bipush 1");
    code("not0:");
    code("");
    code(".method eq_");
    code(".args 3");
    code("iload 1");
    code("iload 2");
    code("isub");
    code("ifeq eq1");
    code("bipush 0");
    code("ireturn");
    code("eq1:");
    code("bipush 1");
    code("ireturn");
    code("");
    code(".method ne_");
    code(".args 3");
    code("iload 1");
    code("iload 2");
    code("isub");
    code("ifeq ne0");
    code("bipush 1");
    code("ireturn");
    code("ne0:");
    code("bipush 0");
    code("ireturn");
    code("");
    code(".method lt_");
    code(".args 3");
    code("iload 1");
    code("iload 2");
    code("isub");
    code("iflt lt1");
    code("bipush 0");
    code("ireturn");
    code("lt1:");
    code("bipush 1");
    code("ireturn");
    code("");
    code(".method gt_");
    code(".args 3");
    code("iload 1");
    code("iload 2");
    code("isub");
    code("iflt gt0");
    code("bipush 1");
    code("ireturn");
    code("gt0:");
    code("bipush 0");
    code("ireturn");
    code("");
    code(".method leq_");
    code(".args 3");
    code("iload 1");
    code("iload 2");
    code("isub");
    code("dup");
    code("ifeq leqpop1");
    code("iflt leq1");
    code("bipush 0");
    code("ireturn");
    code("leq1:");
    code("bipush 1");
    code("ireturn");
    code("leqpop1:");
    code("pop");
    code("bipush 1");
    code("ireturn");
    code("");
    code(".method geq_");
    code(".args 3");
    code("iload 1");
    code("iload 2");
    code("isub");
    code("dup");
    code("ifeq geqpop1");
    code("iflt geq0");
    code("bipush 1");
    code("ireturn");
    code("geq0:");
    code("bipush 0");
    code("ireturn");
    code("geqpop1:");
    code("pop");
    code("bipush 1");
    code("ireturn");
    code("");
  }