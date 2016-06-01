Example: Lambda Calculus, Expressions and Statements
====================================================
The support code generated for this example is based on the following signature:

    Const: Int → Expr
    Add: Expr × Expr → Expr
    Id: String → Expr
    Lambda: String × Expr → Expr
    Call: Expr × Expr → Expr
    Assign: String × Expr → Stmt
    Seq: Stmt × Stmt → Stmt
    ExprStmt: Expr → Stmt

As extension:

    Let: String × Expr × Expr → Expr

The following table illustrates the mapping between names used in the paper
and names in the source files of this example:

| Paper           | This Example                |
|-----------------|-----------------------------|
| `mix`           | `Compose`                   |
| `ExprAssemble`  | `Expr.AttributeGrammar`     |
| `ExprCompose`   | `Expr.Signature.DepProduct` |
| `PreExprSig`    | `Expr.Signature`            |
| `ExprSig`       | `Expr.Syn.Complete`         |
| `InhSig`        | `Expr.Inh.Complete`         |
| `CtxExprSig`    | `Expr.Syn`                  |
| `CtxInhSig`     | `Expr.Inh`                  |