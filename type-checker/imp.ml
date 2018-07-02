(* Danielle Stewart 
   CS 5571
   Assignment 6
   May 7, 2015 
   Type Checker *)

open Imptypes;;
type varname = string

(* IMP types consist of integers and booleans 
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
      | Decl of (ityp * varname)*)


type vartyp =
        Undeclared
      | VTyp of (ityp * bool);;

type typctx = varname -> vartyp;;

type cmdtyp = TypCtx of typctx | CTypErr of string;;

type exprtyp = ExpTyp of ityp | ETypErr of string;;


let update s v i = (fun x -> if x=v then i else (s x));;

let init_typctx (l : (varname*vartyp) list) : typctx =
fun x -> (try (List.assoc x l) with Not_found -> Undeclared);;


(* Another helper that takes in a context and a varname and returns
   the type associated with that varname. It assumes all types are
   declared so it returns TypBool if Undeclared is found. This was just
   to avoid the warning messages I was getting.  *)
let getITyp (tc:typctx) (var:varname) : ityp =
  let (x:vartyp) = tc (var) in
    (match x with
      VTyp(t, _) -> t
    | Undeclared -> TypBool);;



(* Helper function called typchk_op that takes in expected argument type, 
   expected return type of the expressions, and two expressions. *)

let typchk_op (tc:typctx) (atyp:ityp) (rtyp:ityp) (e1:iexpr) (e2:iexpr) : exprtyp =
  (match e1 with
    Const(_) -> if atyp==TypBool then ETypErr("Error Type")
              else (match e2 with
                  Const(_) -> ExpTyp(rtyp)
                | Plus(_,_) -> ExpTyp(rtyp)
                | Minus(_,_) -> ExpTyp(rtyp)
                | Times(_,_) -> ExpTyp(rtyp)
                | Var(v) -> if (getITyp tc v) == TypInt then ExpTyp(rtyp)
                            else ETypErr("Error type")
                | _ -> ETypErr("Error Type"))
  | Var(v) -> if (getITyp tc v) == atyp then
                (match e2 with
                | Const(_) -> if atyp == TypInt then ExpTyp(rtyp)
                              else ETypErr("Error type")
                | Var(v2) -> if (getITyp tc v2) == atyp then ExpTyp(rtyp)
                             else ETypErr("Error type")
                | Plus(_,_) -> if atyp==TypInt then ExpTyp(rtyp)
                               else ETypErr("error type")
                | Minus(_,_) -> if atyp==TypInt then ExpTyp(rtyp)
                               else ETypErr("error type")
                | Times(_,_) -> if atyp==TypInt then ExpTyp(rtyp)
                               else ETypErr("error type")
                | _ -> if atyp == TypBool then ExpTyp(rtyp)
                        else ETypErr("error type"))
              else ETypErr("Error type")
  | Plus(_,_) -> if atyp == TypBool then ETypErr("Error type")
                 else (match e2 with
                  Const(_) -> ExpTyp(rtyp)
                | Plus(_,_) -> ExpTyp(rtyp)
                | Minus(_,_) -> ExpTyp(rtyp)
                | Times(_,_) -> ExpTyp(rtyp)
                | Var(v) -> if (getITyp tc v) == TypInt then ExpTyp(rtyp)
                            else ETypErr("Error Type")
                | _ -> ETypErr("Error Type"))
  | Minus(_,_) -> if atyp == TypBool then ETypErr("Error type")
                 else (match e2 with
                  Const(_) -> ExpTyp(rtyp)
                | Plus(_,_) -> ExpTyp(rtyp)
                | Minus(_,_) -> ExpTyp(rtyp)
                | Times(_,_) -> ExpTyp(rtyp)
                | Var(v) -> if (getITyp tc v) == TypInt then ExpTyp(rtyp)
                            else ETypErr("Error type")
                | _ -> ETypErr("Error Type"))
  | Times(_,_) -> if atyp == TypBool then ETypErr("Error type")
                 else (match e2 with
                  Const(_) -> ExpTyp(rtyp)
                | Plus(_,_) -> ExpTyp(rtyp)
                | Minus(_,_) -> ExpTyp(rtyp)
                | Times(_,_) -> ExpTyp(rtyp)
                | Var(v) -> if (getITyp tc v) == TypInt then ExpTyp(rtyp)
                            else ETypErr("Error type")
                | _ -> ETypErr("Error Type"))
  | _ ->  if atyp == TypInt then ETypErr("Error type")
          else (match e2 with
        | True -> ExpTyp(rtyp)
        | False -> ExpTyp(rtyp)
        | Leq(_,_) -> ExpTyp(rtyp)
        | Disj(_,_) -> ExpTyp(rtyp)
        | Neg(_) -> ExpTyp(rtyp)
        | Var(v) -> if (getITyp tc v) == TypBool then ExpTyp(rtyp)
                    else ETypErr("error type")
        | _ -> ETypErr("Error type")));;





let rec typchk_expr (tc:typctx) (e:iexpr) : exprtyp =
   (match e with
          Const(_) -> ExpTyp(TypInt)
        | Var(v) -> if (getITyp tc v) == TypInt then ExpTyp(TypInt)
                    else ExpTyp(TypBool)
        | Plus(e1,e2) -> typchk_op tc TypInt TypInt e1 e2
        | Minus(e1,e2) -> typchk_op tc TypInt TypInt e1 e2
        | Times(e1,e2) -> typchk_op tc TypInt TypInt e1 e2
        | True -> ExpTyp(TypBool)
        | False -> ExpTyp(TypBool)
        | Leq(e1, e2) -> typchk_op tc TypInt TypBool e1 e2
        | Conj(e1, e2) -> typchk_op tc TypBool TypBool e1 e2
        | Disj(e1, e2) -> typchk_op tc TypBool TypBool e1 e2
        | Neg(e1) -> typchk_expr tc e1);;


  
let rec typchk_cmd (tc:typctx) (c:icmd) : cmdtyp =

  (match c with
    Skip -> TypCtx tc
  | Seq(c1,c2) -> (match typchk_cmd tc c1 with
                    TypCtx tc2 -> typchk_cmd tc2 c2
                  | CTypErr s -> CTypErr("Error in sequential typing"))
  | Assign(v,e1) -> (match (tc v, typchk_expr tc e1) with
                      (VTyp(t1,_), ExpTyp t2) ->
                        if t1=t2 then TypCtx (update tc v (VTyp(t1,true)))
                        else CTypErr("Error in assign, mismatched types")
                    | (_, ExpTyp _)|(_, ETypErr _)  -> CTypErr("Assign on undeclared error"))
  | Cond(e1,c1,c2) -> (match (typchk_expr tc e1, typchk_cmd tc c1, typchk_cmd tc c2) with
                         (ExpTyp TypBool, TypCtx _, TypCtx _) -> TypCtx tc
                       | (ExpTyp TypInt, TypCtx _, TypCtx _)| (_, CTypErr _, _)
                       | (_, _, CTypErr _)|(ETypErr _, TypCtx _, TypCtx _)
                                -> CTypErr("Error in conditional"))
  | While(e1,c1) -> (match typchk_expr tc e1 with
                      ExpTyp TypBool -> typchk_cmd tc c1
                    | _ -> CTypErr("Error in while typing"))
  | Decl(t1, var) -> (match (t1, tc var) with
                       (TypInt, Undeclared) -> TypCtx (update tc var (VTyp(TypInt, false)))
                     | (TypBool, Undeclared) -> TypCtx (update tc var (VTyp(TypBool, false)))
                     | (_, _) -> CTypErr("Error in declaration typing")));;
  


(* NO CHANGES REQUIRED AFTER THIS POINT
 * The following code is essentially the same as the interpreter you wrote
 * for assignment 1, except modified to interpret TIMP programs instead of
 * IMP programs.
 *)

type store = varname -> int

let init_store (l : (varname*int) list) : store =
  fun x -> List.assoc x l;;

let rec eval_expr (s:store) (e:iexpr) : int =
  (match e with
     Const n -> n
   | Var x -> (s x)
   | Plus (e1,e2)  | Disj (e1,e2) -> (eval_expr s e1) + (eval_expr s e2)
   | Minus (e1,e2) -> (eval_expr s e1) - (eval_expr s e2)
   | Times (e1,e2) | Conj (e1,e2) -> (eval_expr s e1) * (eval_expr s e2)
   | True -> 1
   | False -> 0
   | Leq (e1,e2) -> if (eval_expr s e1) <= (eval_expr s e2) then 1 else 0
   | Neg e1 -> if (eval_expr s e1)=0 then 1 else 0
  );;

let rec exec_cmd (s:store) (c:icmd) : store =
  (match c with
     Skip | Decl _ -> s
   | Seq (c1,c2) -> exec_cmd (exec_cmd s c1) c2
   | Assign (v,e) -> update s v (eval_expr s e)
   | Cond (e,c1,c2) -> exec_cmd s (if (eval_expr s e)=0 then c2 else c1)
   | While (e,c1) -> exec_cmd s (Cond (e,Seq (c1,c),Skip))
  );;


(* The main function now calls typchk_cmd (your code) before attempting to
 * execute the IMP program. Only well-typed IMP programs are executed.
 *)
let main () =
   let argval = (function "true" -> 1 | "false" -> 0 | x -> int_of_string x) in
   let argtyp = (function "true" | "false" -> TypBool | _ -> TypInt) in
   let c = (Impparser.parse_cmd Implexer.token 
              (Lexing.from_channel (open_in Sys.argv.(1)))) in
   let s = init_store (List.tl (List.tl (Array.to_list (Array.mapi
             (fun i a -> ("arg"^(string_of_int (i-2)),
                          if i>=2 then (argval a) else 0))
             Sys.argv)))) in
   let tc = init_typctx (List.tl (List.tl (Array.to_list (Array.mapi
             (fun i a -> ("arg"^(string_of_int (i-2)), VTyp (argtyp a,true)))
             Sys.argv)))) in
   (match typchk_cmd tc c with
      CTypErr s -> print_string ("Typing error: "^s^"\n")
    | TypCtx _ -> let s2 = exec_cmd s c in
                    (print_int (s2 "ret"); print_newline ()));;

main ();;


