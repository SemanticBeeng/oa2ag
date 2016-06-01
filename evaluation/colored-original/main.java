  public static void main(String[] args) {
    try {
      tFile = args[0];
      in = new FileReader(tFile);
      out = new FileWriter(tFile.substring(0,tFile.lastIndexOf('.'))+".j");
      tLine = 1;
      tCol = 0;
      codeLibrary();
      nextChar();
      nextToken();
      parseProgram();
      out.close();
    } catch (Exception e) { e.printStackTrace(); }
  }
