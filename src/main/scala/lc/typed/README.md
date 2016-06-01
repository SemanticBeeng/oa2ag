Example: Simply Typed Lambda Calculus
=====================================
The support code generated for this example is based on the following signature:

    Const: Int → Expr
    Add: Expr × Expr → Expr
    Id: String → Expr
    Lambda: String × Type × Expr → Expr
    Call: Expr × Expr → Expr
    IntT: Type
    ArrowT: Type × Type → Type

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