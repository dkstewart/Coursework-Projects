type-checker

Lexer, parser, evaluator, and type-checker for a language called TIMP (in OCaml)

Initial Build
----------------------------------------------
ocamlc-c imptypes.ml

ocamlyacc impparser.mly

ocamlc -c impparser.mli

ocamlc -c impparser.ml

ocamllex implexer.mll

ocamlc -c implexer.ml

ocamlc -c imp.ml

ocamlc -o imp.exe imptypes.cmo implexer.cmo impparser.cmo imp.cmo

Type descriptions
----------------------------------------------
IMP types consist of integers and booleans (TypeInt, TypBool)

The iexpr type includes both arithmatic and boolean types (Const, Var, Plus, Minus, Times, True, False, Leq, Conj, Disj, Neg)

The icmd type consists of commands (Skip, Seq, Assign, Cond, While, Decl)

For more information on types, see imptypes.ml.