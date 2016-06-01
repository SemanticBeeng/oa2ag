Example: Type Reconstruction
============================
The support code generated for this example is based on the following signature:

    VarExpr: String → Expr
    AbsExpr: String × Expr → Expr
    AppExpr: Expr × Expr → Expr
    NumExpr: Int → Expr
    SuccExpr: Expr → Expr
    PredExpr: Expr → Expr
    IsZeroExpr: Expr → Expr
    BoolExpr: Boolean → Expr
    IfExpr: Expr × Expr × Expr → Expr

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