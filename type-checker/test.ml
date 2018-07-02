
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

type icmd =
        Skip
      | Seq of (icmd * icmd)
      | Assign of (varname * iexpr)
      | Cond of (iexpr * icmd * icmd)
      | While of (iexpr * icmd)
      | Decl of (ityp * varname)


type vartyp =
        Undeclared
      | VTyp of (ityp * bool);;

type typctx = varname -> vartyp;;

type cmdtyp = TypCtx of typctx | CTypErr of string;;

type exprtyp = ExpTyp of ityp | ETypErr of string;;

(* Another helper *)
let getITyp (tc:typctx) (var:varname) : ityp =
  let (x:vartyp) = tc (var) in 
    (match x with 
      VTyp(t, _) -> t
    | Undeclared -> TypBool);;


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



let (x:iexpr) = Const(5);;

let (y:iexpr) = False;;

let list = let (var1:varname) = "var1" in [(var1,VTyp(TypInt,false))];;

let init_typctx (l : (varname*vartyp) list) : typctx =
fun x -> (try (List.assoc x l) with Not_found -> Undeclared);;

let update s v i = (fun x -> if x=v then i else (s x));;


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
		  | _ -> CTypErr("Error in sequential typing"))
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
		     | (_, _) -> CTypErr("Error in declaration typing")))
