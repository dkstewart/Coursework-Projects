type token =
  | LPAREN
  | RPAREN
  | SEMICOLON
  | ASSIGN
  | LEQ
  | OR
  | AND
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | EOF
  | SKIP
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | TRUE
  | FALSE
  | INT
  | BOOL
  | NUM of (int)
  | VAR of (string)

open Parsing;;
let _ = parse_error;;
# 2 "impparser.mly"
  open Imptypes
  open Parsing
  open Lexing
  let report_error pos1 pos2 msg =
    failwith ("Line "^(string_of_int pos1.pos_lnum)^", char"^
      (if pos1.pos_cnum = pos2.pos_cnum then
         " "^(string_of_int (pos1.pos_cnum - pos1.pos_bol + 1))
       else
         ("s "^(string_of_int (pos1.pos_cnum - pos1.pos_bol + 1))^
          "-"^(string_of_int (pos2.pos_cnum - pos1.pos_bol + 1))))^": "^msg);;
  let unmatched opening_name opening_num closing_name =
    report_error (rhs_start_pos opening_num) (rhs_end_pos opening_num)
      ("Found "^opening_name^" with no matching "^closing_name^".");;
  let invalid num = report_error (rhs_start_pos num) (rhs_end_pos num)
  let parse_error msg =
    report_error (symbol_start_pos ()) (symbol_end_pos ()) msg;;
  let missing num =
    report_error (rhs_end_pos num) (rhs_end_pos num) "missing semicolon";;
# 49 "impparser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* SEMICOLON *);
  260 (* ASSIGN *);
  261 (* LEQ *);
  262 (* OR *);
  263 (* AND *);
  264 (* NOT *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
    0 (* EOF *);
  268 (* SKIP *);
  269 (* IF *);
  270 (* THEN *);
  271 (* ELSE *);
  272 (* WHILE *);
  273 (* DO *);
  274 (* TRUE *);
  275 (* FALSE *);
  276 (* INT *);
  277 (* BOOL *);
    0|]

let yytransl_block = [|
  278 (* NUM *);
  279 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\001\000\003\000\002\000\002\000\001\000\003\000\003\000\001\000\
\003\000\006\000\005\000\003\000\004\000\003\000\002\000\002\000\
\001\000\001\000\003\000\003\000\003\000\002\000\001\000\002\000\
\001\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\000\
\000\000\031\000\000\000\005\000\000\000\000\000\000\000\000\000\
\017\000\018\000\023\000\025\000\000\000\000\000\015\000\016\000\
\000\000\000\000\004\000\007\000\006\000\000\000\000\000\024\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\000\000\000\000\002\000\030\000\029\000\000\000\000\000\
\000\000\000\000\000\000\028\000\000\000\013\000\011\000\000\000\
\010\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\021\000"

let yysindex = "\006\000\
\023\255\000\000\023\255\000\000\039\255\039\255\246\254\250\254\
\017\255\000\000\052\255\000\000\115\255\039\255\039\255\004\255\
\000\000\000\000\000\000\000\000\071\255\005\255\000\000\000\000\
\039\255\023\255\000\000\000\000\000\000\086\255\096\255\000\000\
\000\000\039\255\039\255\039\255\039\255\039\255\039\255\023\255\
\000\000\023\255\093\255\000\000\000\000\000\000\041\255\103\255\
\096\255\016\255\016\255\000\000\003\255\000\000\000\000\023\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\053\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\054\000\000\000\000\000\000\000\000\000\045\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\133\000\000\000\000\000\000\000\067\000\111\000\
\089\000\001\000\023\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\255\255\220\255\026\000\250\255"

let yytablesize = 412
let yytable = "\022\000\
\026\000\013\000\055\000\053\000\041\000\054\000\001\000\030\000\
\031\000\034\000\035\000\036\000\023\000\037\000\038\000\039\000\
\024\000\056\000\043\000\057\000\025\000\042\000\027\000\003\000\
\044\000\032\000\039\000\047\000\048\000\049\000\050\000\051\000\
\052\000\000\000\004\000\005\000\027\000\000\000\006\000\014\000\
\000\000\000\000\007\000\008\000\022\000\009\000\015\000\000\000\
\016\000\037\000\038\000\039\000\001\000\003\000\026\000\000\000\
\017\000\018\000\000\000\000\000\019\000\020\000\000\000\004\000\
\005\000\000\000\019\000\006\000\000\000\000\000\033\000\007\000\
\008\000\000\000\009\000\034\000\035\000\036\000\000\000\037\000\
\038\000\039\000\000\000\000\000\040\000\045\000\000\000\046\000\
\020\000\000\000\034\000\035\000\036\000\000\000\037\000\038\000\
\039\000\034\000\035\000\036\000\034\000\037\000\038\000\039\000\
\037\000\038\000\039\000\034\000\000\000\036\000\021\000\037\000\
\038\000\039\000\028\000\000\000\029\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\000\000\000\026\000\026\000\000\000\026\000\026\000\026\000\
\000\000\026\000\026\000\000\000\026\000\026\000\026\000\026\000\
\026\000\026\000\000\000\000\000\026\000\026\000\027\000\026\000\
\027\000\027\000\000\000\027\000\027\000\027\000\000\000\027\000\
\027\000\000\000\027\000\027\000\027\000\027\000\027\000\027\000\
\000\000\000\000\027\000\027\000\022\000\027\000\022\000\022\000\
\000\000\000\000\022\000\022\000\001\000\003\000\001\000\003\000\
\022\000\022\000\022\000\022\000\022\000\022\000\000\000\000\000\
\022\000\022\000\019\000\022\000\019\000\019\000\000\000\000\000\
\019\000\019\000\000\000\000\000\000\000\000\000\019\000\019\000\
\019\000\019\000\019\000\019\000\000\000\000\000\019\000\019\000\
\020\000\019\000\020\000\020\000\000\000\000\000\020\000\020\000\
\000\000\000\000\000\000\000\000\020\000\020\000\020\000\020\000\
\020\000\020\000\000\000\000\000\020\000\020\000\021\000\020\000\
\021\000\021\000\000\000\000\000\021\000\000\000\000\000\000\000\
\000\000\000\000\021\000\021\000\021\000\021\000\021\000\021\000\
\000\000\000\000\021\000\021\000\009\000\021\000\009\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\009\000\009\000\000\000\009\000\009\000\000\000\000\000\000\000\
\009\000\009\000\000\000\009\000"

let yycheck = "\006\000\
\000\000\003\000\000\001\040\000\000\001\042\000\001\000\014\000\
\015\000\005\001\006\001\007\001\023\001\009\001\010\001\011\001\
\023\001\015\001\025\000\056\000\004\001\017\001\000\000\001\001\
\026\000\022\001\011\001\034\000\035\000\036\000\037\000\038\000\
\039\000\255\255\012\001\013\001\011\000\255\255\016\001\001\001\
\255\255\255\255\020\001\021\001\000\000\023\001\008\001\255\255\
\010\001\009\001\010\001\011\001\000\000\000\000\003\001\255\255\
\018\001\019\001\255\255\255\255\022\001\023\001\255\255\012\001\
\013\001\255\255\000\000\016\001\255\255\255\255\000\001\020\001\
\021\001\255\255\023\001\005\001\006\001\007\001\255\255\009\001\
\010\001\011\001\255\255\255\255\014\001\000\001\255\255\002\001\
\000\000\255\255\005\001\006\001\007\001\255\255\009\001\010\001\
\011\001\005\001\006\001\007\001\005\001\009\001\010\001\011\001\
\009\001\010\001\011\001\005\001\255\255\007\001\000\000\009\001\
\010\001\011\001\000\001\255\255\002\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\255\255\002\001\003\001\255\255\005\001\006\001\007\001\
\255\255\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\255\255\255\255\020\001\021\001\000\001\023\001\
\002\001\003\001\255\255\005\001\006\001\007\001\255\255\009\001\
\010\001\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\255\255\255\255\020\001\021\001\000\001\023\001\002\001\003\001\
\255\255\255\255\006\001\007\001\000\001\000\001\002\001\002\001\
\012\001\013\001\014\001\015\001\016\001\017\001\255\255\255\255\
\020\001\021\001\000\001\023\001\002\001\003\001\255\255\255\255\
\006\001\007\001\255\255\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\255\255\255\255\020\001\021\001\
\000\001\023\001\002\001\003\001\255\255\255\255\006\001\007\001\
\255\255\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\255\255\255\255\020\001\021\001\000\001\023\001\
\002\001\003\001\255\255\255\255\006\001\255\255\255\255\255\255\
\255\255\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\255\255\255\255\020\001\021\001\000\001\023\001\002\001\003\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\012\001\013\001\255\255\015\001\016\001\255\255\255\255\255\255\
\020\001\021\001\255\255\023\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  SEMICOLON\000\
  ASSIGN\000\
  LEQ\000\
  OR\000\
  AND\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  EOF\000\
  SKIP\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  DO\000\
  TRUE\000\
  FALSE\000\
  INT\000\
  BOOL\000\
  "

let yynames_block = "\
  NUM\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 46 "impparser.mly"
               ( _1 )
# 274 "impparser.ml"
               : Imptypes.icmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Imptypes.icmd) in
    Obj.repr(
# 47 "impparser.mly"
                                   ( Seq (_1,_3) )
# 282 "impparser.ml"
               : Imptypes.icmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 48 "impparser.mly"
                         ( invalid 2 "superfluous semicolon" )
# 289 "impparser.ml"
               : Imptypes.icmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simplecmd) in
    Obj.repr(
# 49 "impparser.mly"
                         ( missing 1 )
# 297 "impparser.ml"
               : Imptypes.icmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simplecmd) in
    Obj.repr(
# 52 "impparser.mly"
               ( _1 )
# 304 "impparser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Imptypes.icmd) in
    Obj.repr(
# 53 "impparser.mly"
                             ( _2 )
# 311 "impparser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Imptypes.icmd) in
    Obj.repr(
# 54 "impparser.mly"
                            ( unmatched "(" 1 ")" )
# 318 "impparser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "impparser.mly"
                ( Skip )
# 324 "impparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 58 "impparser.mly"
                                 ( Assign (_1,_3) )
# 332 "impparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'parse_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'cmd) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 59 "impparser.mly"
                                           ( Cond (_2,_4,_6) )
# 341 "impparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'parse_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 60 "impparser.mly"
                                        ( unmatched "if" 1 "else" )
# 349 "impparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'parse_expr) in
    Obj.repr(
# 61 "impparser.mly"
                               ( unmatched "if" 1 "then" )
# 356 "impparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 62 "impparser.mly"
                                   ( While (_2,_4) )
# 364 "impparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'parse_expr) in
    Obj.repr(
# 63 "impparser.mly"
                                  ( unmatched "while" 1 "do" )
# 371 "impparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "impparser.mly"
                   ( Decl (TypInt,_2) )
# 378 "impparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "impparser.mly"
                    ( Decl (TypBool,_2) )
# 385 "impparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "impparser.mly"
                 ( True )
# 391 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "impparser.mly"
                  ( False )
# 397 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 70 "impparser.mly"
                                      ( Leq (_1,_3) )
# 405 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 71 "impparser.mly"
                                      ( Conj (_1,_3) )
# 413 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 72 "impparser.mly"
                                     ( Disj (_1,_3) )
# 421 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 73 "impparser.mly"
                           ( Neg _2 )
# 428 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "impparser.mly"
                ( Const _1 )
# 435 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 75 "impparser.mly"
                      ( Const (-_2) )
# 442 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "impparser.mly"
                ( Var _1 )
# 449 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 77 "impparser.mly"
                                       ( Plus (_1,_3) )
# 457 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 78 "impparser.mly"
                                        ( Minus (_1,_3) )
# 465 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 79 "impparser.mly"
                                        ( Times (_1,_3) )
# 473 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'parse_expr) in
    Obj.repr(
# 80 "impparser.mly"
                                     ( _2 )
# 480 "impparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'parse_expr) in
    Obj.repr(
# 81 "impparser.mly"
                                    ( unmatched "(" 1 ")" )
# 487 "impparser.ml"
               : 'parse_expr))
(* Entry parse_cmd *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parse_cmd (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Imptypes.icmd)
