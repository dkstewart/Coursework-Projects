type varname = string

(* IMP types consist of integers and booleans *)
type ityp =
        TypInt
      | TypBool

(* The new iexpr type merges the iarith and ibool types
 * from the previous assignment.
 *)
type iexpr =
        Const of int
      | Var of varname
      | Plus of (iexpr * iexpr)
      | Minus of (iexpr * iexpr)
      | Times of (iexpr * iexpr)
      | True
      | False
      | Leq of (iexpr * iexpr)
      | Conj of (iexpr * iexpr)
      | Disj of (iexpr * iexpr)
      | Neg of iexpr

(* The icmd type is the same as before, except that all
 * iarith and ibool clauses have been changed to iexpr
 * clauses.  There is also a new Decl command that declares
 * a new IMP program variable.
 *)
type icmd =
        Skip
      | Seq of (icmd * icmd)
      | Assign of (varname * iexpr)
      | Cond of (iexpr * icmd * icmd)
      | While of (iexpr * icmd)
      | Decl of (ityp * varname)
