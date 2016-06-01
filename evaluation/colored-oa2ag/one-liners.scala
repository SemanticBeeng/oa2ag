// taken from `expression/compiler.scala`
Expression.Inh.Default[HasFunctions with HasVariables, Any, HasFunctions with HasVariables]

// taken from `function/compiler.scala`
Function.Inh.Default[Any, Any, HasFunctions, Any, HasFunctions]

// taken from `lists/compiler.scala`
ConsList.Inh.Default[Any, HasFunctions with HasVariables, Any, HasFunctions with HasVariables]
ConsList.Inh.Forward[HasVariables]

// taken from `statement/compiler.scala`
Statement.Inh.Default[Any, FunVar, Any, FunVar, Any, FunVar]
ConsList.Inh.Default[Any, HasFunctions with HasVariables, Any, HasFunctions with HasVariables]
