(*****************************************************************************
 * Copyright (c) 2008, Eric Breck
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * 1 Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * 
 * 2 Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * 
 * 3 The name of the author may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *****************************************************************************)
type token =
  | OBRACKET
  | CLOBRACKET
  | EQUALS
  | SEMI
  | BLANK
  | EOF
  | RANGLE
  | IN
  | EOL
  | ROOT
  | LANGLE
  | OPAREN
  | CLOPAREN
  | QUOTE
  | AST
  | SPC
  | RANGLE_OPAREN
  | ID of (string)
  | STR of (string)
  | VAR of (string)
  | SUFFIX of (string)
  | DEF of (string)
  | COMMENT of (string)
  | INT of (int)

open Parsing;;
# 2 "parser.mly"
  open Types
  open Lexing
  (* get the line number of the start of the nth token on the RHS of the rule *)
  let linenum n = (rhs_start_pos n).pos_lnum
# 34 "parser.ml"
let yytransl_const = [|
  257 (* OBRACKET *);
  258 (* CLOBRACKET *);
  259 (* EQUALS *);
  260 (* SEMI *);
  261 (* BLANK *);
    0 (* EOF *);
  262 (* RANGLE *);
  263 (* IN *);
  264 (* EOL *);
  265 (* ROOT *);
  266 (* LANGLE *);
  267 (* OPAREN *);
  268 (* CLOPAREN *);
  269 (* QUOTE *);
  270 (* AST *);
  271 (* SPC *);
  272 (* RANGLE_OPAREN *);
    0|]

let yytransl_block = [|
  273 (* ID *);
  274 (* STR *);
  275 (* VAR *);
  276 (* SUFFIX *);
  277 (* DEF *);
  278 (* COMMENT *);
  279 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\005\000\007\000\007\000\007\000\007\000\008\000\
\008\000\004\000\004\000\004\000\011\000\011\000\013\000\013\000\
\009\000\009\000\009\000\014\000\014\000\002\000\002\000\006\000\
\006\000\010\000\012\000\012\000\017\000\017\000\015\000\015\000\
\015\000\018\000\021\000\021\000\016\000\020\000\020\000\019\000\
\019\000\019\000\022\000\022\000\022\000\022\000\022\000\023\000\
\023\000\024\000\025\000\025\000\003\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\002\000\003\000\002\000\001\000\001\000\
\002\000\001\000\001\000\000\000\001\000\001\000\001\000\002\000\
\001\000\002\000\002\000\002\000\003\000\001\000\002\000\001\000\
\002\000\002\000\001\000\001\000\001\000\001\000\003\000\003\000\
\002\000\002\000\002\000\003\000\003\000\001\000\001\000\000\000\
\004\000\005\000\001\000\001\000\001\000\003\000\002\000\000\000\
\002\000\005\000\000\000\002\000\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\017\000\029\000\030\000\013\000\
\054\000\007\000\000\000\000\000\000\000\000\000\000\000\014\000\
\000\000\000\000\027\000\028\000\000\000\000\000\055\000\000\000\
\056\000\000\000\000\000\001\000\000\000\000\000\000\000\004\000\
\008\000\018\000\002\000\019\000\006\000\016\000\020\000\000\000\
\023\000\000\000\000\000\000\000\000\000\000\000\000\000\045\000\
\044\000\033\000\000\000\035\000\043\000\000\000\000\000\000\000\
\052\000\053\000\026\000\025\000\005\000\009\000\021\000\000\000\
\031\000\032\000\000\000\047\000\036\000\000\000\038\000\039\000\
\034\000\049\000\037\000\000\000\046\000\000\000\000\000\000\000\
\000\000\041\000\050\000\042\000"

let yydgoto = "\004\000\
\009\000\010\000\025\000\011\000\012\000\031\000\013\000\032\000\
\014\000\033\000\015\000\016\000\017\000\018\000\019\000\020\000\
\021\000\050\000\051\000\073\000\022\000\054\000\055\000\026\000\
\027\000"

let yysindex = "\031\000\
\001\255\034\255\012\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\002\000\041\255\034\255\034\255\000\000\
\027\255\034\255\000\000\000\000\030\255\063\255\000\000\028\255\
\000\000\012\255\056\000\000\000\034\255\049\255\253\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\255\
\000\000\058\255\058\255\063\255\063\255\063\255\074\255\000\000\
\000\000\000\000\008\255\000\000\000\000\063\255\008\255\062\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\074\255\
\000\000\000\000\070\255\000\000\000\000\055\255\000\000\000\000\
\000\000\000\000\000\000\063\255\000\000\063\255\058\255\075\255\
\058\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\005\000\000\000\084\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\009\000\046\255\000\000\
\000\000\001\000\000\000\000\000\011\255\018\255\000\000\000\000\
\000\000\084\000\000\000\000\000\000\000\027\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\255\011\255\073\255\000\000\000\000\047\255\000\000\
\000\000\000\000\000\000\000\000\000\000\018\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\011\255\000\000\
\011\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\008\000\000\000\000\000\000\000\057\000\000\000\000\000\
\000\000\058\000\000\000\000\000\073\000\050\000\000\000\000\000\
\000\000\020\000\191\255\036\000\000\000\235\255\223\255\000\000\
\066\000"

let yytablesize = 304
let yytable = "\052\000\
\022\000\035\000\028\000\003\000\012\000\005\000\010\000\006\000\
\011\000\023\000\067\000\006\000\007\000\082\000\008\000\084\000\
\007\000\029\000\008\000\071\000\074\000\037\000\040\000\068\000\
\069\000\041\000\024\000\072\000\024\000\048\000\040\000\001\000\
\002\000\003\000\039\000\042\000\059\000\048\000\061\000\043\000\
\044\000\040\000\045\000\046\000\006\000\036\000\047\000\048\000\
\079\000\007\000\056\000\008\000\049\000\015\000\080\000\058\000\
\081\000\043\000\043\000\043\000\015\000\065\000\066\000\043\000\
\043\000\044\000\043\000\045\000\078\000\043\000\030\000\053\000\
\048\000\044\000\064\000\045\000\070\000\049\000\076\000\053\000\
\048\000\077\000\083\000\051\000\048\000\049\000\060\000\038\000\
\062\000\063\000\075\000\057\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\034\000\000\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\022\000\029\000\
\030\000\012\000\012\000\010\000\010\000\011\000\011\000\024\000\
\000\000\000\000\000\000\000\000\000\000\024\000\000\000\000\000\
\000\000\000\000\024\000\000\000\024\000\000\000\000\000\024\000"

let yycheck = "\021\000\
\000\000\000\000\000\000\000\000\000\000\005\001\000\000\011\001\
\000\000\002\000\044\000\011\001\016\001\079\000\018\001\081\000\
\016\001\021\001\018\001\012\001\054\000\014\000\012\001\045\000\
\046\000\018\000\000\000\020\001\017\001\012\001\020\001\001\000\
\002\000\003\000\008\001\006\001\029\000\020\001\031\000\010\001\
\011\001\015\001\013\001\014\001\011\001\005\001\017\001\018\001\
\070\000\016\001\023\001\018\001\023\001\008\001\076\000\000\000\
\078\000\011\001\012\001\013\001\015\001\042\000\043\000\017\001\
\018\001\011\001\020\001\013\001\014\001\023\001\022\001\017\001\
\018\001\011\001\017\001\013\001\003\001\023\001\017\001\017\001\
\018\001\012\001\008\001\000\000\012\001\023\001\030\000\015\000\
\031\000\040\000\055\000\026\000\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\005\001\005\001\255\255\
\005\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\021\001\022\001\021\001\
\022\001\021\001\022\001\021\001\022\001\021\001\022\001\005\001\
\255\255\255\255\255\255\255\255\255\255\011\001\255\255\255\255\
\255\255\255\255\016\001\255\255\018\001\255\255\255\255\021\001"

let yynames_const = "\
  OBRACKET\000\
  CLOBRACKET\000\
  EQUALS\000\
  SEMI\000\
  BLANK\000\
  EOF\000\
  RANGLE\000\
  IN\000\
  EOL\000\
  ROOT\000\
  LANGLE\000\
  OPAREN\000\
  CLOPAREN\000\
  QUOTE\000\
  AST\000\
  SPC\000\
  RANGLE_OPAREN\000\
  "

let yynames_block = "\
  ID\000\
  STR\000\
  VAR\000\
  SUFFIX\000\
  DEF\000\
  COMMENT\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'end_in_not_comment) in
    Obj.repr(
# 18 "parser.mly"
                               (List.rev _1)
# 252 "parser.ml"
               : Types.group list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'end_in_comment) in
    Obj.repr(
# 18 "parser.mly"
                                                                  (List.rev _1)
# 259 "parser.ml"
               : Types.group list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'end_in_not_comment) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'comments) in
    Obj.repr(
# 20 "parser.mly"
                                            (_1)
# 267 "parser.ml"
               : 'end_in_comment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'end_in_not_comment) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ct_def) in
    Obj.repr(
# 23 "parser.mly"
                            (let n,v,ln = _2 in Def (n,v,ln)::_1)
# 275 "parser.ml"
               : 'end_in_topexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'end_in_not_comment) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'comments) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Types.rawkvs,string Types.expr * Types.splat) Types.topexpr list list) in
    Obj.repr(
# 24 "parser.mly"
                                       (
  Rule((String.concat "\n" _2),_3,linenum 2)::_1
)
# 286 "parser.ml"
               : 'end_in_topexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'end_in_blank) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (Types.rawkvs,string Types.expr * Types.splat) Types.topexpr list list) in
    Obj.repr(
# 27 "parser.mly"
                        (Rule("",_2,linenum 2)::_1)
# 294 "parser.ml"
               : 'end_in_topexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : (Types.rawkvs,string Types.expr * Types.splat) Types.topexpr list list) in
    Obj.repr(
# 28 "parser.mly"
           ([Rule("",_1,linenum 1)])
# 301 "parser.ml"
               : 'end_in_topexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'def) in
    Obj.repr(
# 30 "parser.mly"
            (_1)
# 308 "parser.ml"
               : 'ct_def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'comments) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'def) in
    Obj.repr(
# 30 "parser.mly"
                                (_2)
# 316 "parser.ml"
               : 'ct_def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'end_in_topexpr) in
    Obj.repr(
# 32 "parser.mly"
                                   (_1)
# 323 "parser.ml"
               : 'end_in_not_comment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'end_in_blank) in
    Obj.repr(
# 32 "parser.mly"
                                                       (_1)
# 330 "parser.ml"
               : 'end_in_not_comment))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
                                                              ([])
# 336 "parser.ml"
               : 'end_in_not_comment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "parser.mly"
      ([Expr (Str _1,Flat)])
# 343 "parser.ml"
               : 'topexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'interpolation) in
    Obj.repr(
# 36 "parser.mly"
                (_1)
# 350 "parser.ml"
               : 'topexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'topexpr) in
    Obj.repr(
# 39 "parser.mly"
          (_1)
# 357 "parser.ml"
               : 'word))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'word) in
    Obj.repr(
# 40 "parser.mly"
               (_1 @ _2)
# 365 "parser.ml"
               : 'word))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
        ([])
# 371 "parser.ml"
               : 'end_in_blank))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'end_in_comment) in
    Obj.repr(
# 44 "parser.mly"
                       (_1)
# 378 "parser.ml"
               : 'end_in_blank))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'end_in_topexpr) in
    Obj.repr(
# 45 "parser.mly"
                       (_1)
# 385 "parser.ml"
               : 'end_in_blank))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'word) in
    Obj.repr(
# 48 "parser.mly"
           ([_1])
# 392 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'word) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 49 "parser.mly"
                (_1 :: _3)
# 400 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 52 "parser.mly"
       (_1)
# 407 "parser.ml"
               : (Types.rawkvs,string Types.expr * Types.splat) Types.topexpr list list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'line) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (Types.rawkvs,string Types.expr * Types.splat) Types.topexpr list list) in
    Obj.repr(
# 53 "parser.mly"
                (_1 @ _2)
# 415 "parser.ml"
               : (Types.rawkvs,string Types.expr * Types.splat) Types.topexpr list list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                  ([_1])
# 422 "parser.ml"
               : 'comments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'comments) in
    Obj.repr(
# 55 "parser.mly"
                                            (_1 :: _2)
# 430 "parser.ml"
               : 'comments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (Types.rawkvs,string Types.expr * Types.splat) Types.topexpr list list) in
    Obj.repr(
# 57 "parser.mly"
                  (_1,_2,linenum 1)
# 438 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'node_interpolation) in
    Obj.repr(
# 60 "parser.mly"
                     ([ Node _1] )
# 445 "parser.ml"
               : 'interpolation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list_interpolation) in
    Obj.repr(
# 61 "parser.mly"
                       (_1)
# 452 "parser.ml"
               : 'interpolation))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
              (Input)
# 458 "parser.ml"
               : 'start))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                (Output)
# 464 "parser.ml"
               : 'start))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'start) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'node_end) in
    Obj.repr(
# 66 "parser.mly"
                                          ( {_3 with io = Output} )
# 472 "parser.ml"
               : 'node_interpolation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'start) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'node_end) in
    Obj.repr(
# 67 "parser.mly"
                                          ( {_3 with io = Input } )
# 480 "parser.ml"
               : 'node_interpolation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'start) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'node_end) in
    Obj.repr(
# 68 "parser.mly"
                                          ( {_2 with io = _1    } )
# 488 "parser.ml"
               : 'node_interpolation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'kvl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'suffix) in
    Obj.repr(
# 70 "parser.mly"
                          ( {suffix=_2; kvs = _1; io = Input} )
# 496 "parser.ml"
               : 'node_end))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'start) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 73 "parser.mly"
              (Flat,_2)
# 504 "parser.ml"
               : 'list_interp_start))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'start) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 74 "parser.mly"
                  (Splat,_3)
# 512 "parser.ml"
               : 'list_interp_start))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'list_interp_start) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'suffix) in
    Obj.repr(
# 76 "parser.mly"
                                                        (
  let splat, first = _1 in
  (* $(foo).suffix is two separate expressions: $(foo) and ".suffix" *)
  let suffix = match _3 with None -> [] | Some s -> [Str ("." ^ s)] in
  (* $(foo) is the expression foo; $(foo bar) is a function call *)
  let value = match _2 with [] (* one value *) -> first
  | _ :: _ -> List (first::_2) (* several values *) in
  List.map (fun e -> Expr (e,splat) ) (value::suffix)
)
# 529 "parser.ml"
               : 'list_interpolation))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                 (None)
# 535 "parser.ml"
               : 'suffix))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
                                 (Some _1)
# 542 "parser.ml"
               : 'suffix))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
  ([])
# 548 "parser.ml"
               : 'kvl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'kvl_list) in
    Obj.repr(
# 90 "parser.mly"
                           ((_1,_3,Flat)::_4)
# 557 "parser.ml"
               : 'kvl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'kvl_list) in
    Obj.repr(
# 91 "parser.mly"
                               ((_1,_4,Splat)::_5)
# 566 "parser.ml"
               : 'kvl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
     (Var _1)
# 573 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 95 "parser.mly"
      (Int _1)
# 580 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
      (Str _1)
# 587 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 97 "parser.mly"
                             (List _2)
# 594 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 98 "parser.mly"
              (List [Var "quote"; _2])
# 601 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
            ([])
# 607 "parser.ml"
               : 'value_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'value_list) in
    Obj.repr(
# 100 "parser.mly"
                                    (_1::_2)
# 615 "parser.ml"
               : 'value_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    Obj.repr(
# 102 "parser.mly"
                                     (_1,_2,_3,_4)
# 625 "parser.ml"
               : 'dictionary_line))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                  ([])
# 631 "parser.ml"
               : 'dictionary_lines))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'dictionary_line) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dictionary_lines) in
    Obj.repr(
# 103 "parser.mly"
                                                          (_1::_2)
# 639 "parser.ml"
               : 'dictionary_lines))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'dictionary_lines) in
    Obj.repr(
# 104 "parser.mly"
                                 (_1)
# 646 "parser.ml"
               : (string*int*string*string Types.expr) list))
(* Entry groups *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry sentence *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry dictionary *)
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
let groups (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Types.group list)
let sentence (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : (Types.rawkvs,string Types.expr * Types.splat) Types.topexpr list list)
let dictionary (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : (string*int*string*string Types.expr) list)
