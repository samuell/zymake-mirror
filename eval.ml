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
open Types
open Util
open Printf

(* 
A simple scheme/lisp-like language, with a few special features for embedding
in zymake.
*)

exception InvalidArgs 
exception NotAFunction
exception UndeclaredVariable of string
exception EvalEmptyList

(** produces a string representation of an expression for debugging *)
let rec _repr keys b = function
  | Int i -> bprintf b "%d" i
  | Bool v -> bprintf b "%B" v
  | Str s -> bprintf b "\"%s\"" s
  | List l -> Buffer.add_char b '('; List.iter (_reprspc keys b) l; Buffer.add_char b ')'
  | Var (i,s) -> Buffer.add_string b keys.(i)
  | Fun (_,f,e) -> _repr keys b e
and _reprspc keys b e = _repr keys b e; Buffer.add_char b ' '

let repr keys e = let b = Buffer.create 10 in _repr keys b e; Buffer.contents b

(* FIXME: maybe this should use buffers... *)
(** convert an expression into a string for use on the command-line *)
let rec string_of_expr = function
| Int i -> string_of_int i
| Str s -> s
| Bool b -> string_of_bool b
| (List l) -> String.concat " " (List.map string_of_expr l) 
| Var (v,s) -> s
| Fun (s,f,e) -> string_of_expr e

let rec eval lookup = function
| (Int _ | Str _ | Fun _ | Bool _ ) as e -> e (* atoms, self-evaluate *)
| List [] -> raise EvalEmptyList
| List (f :: args) as original_expression ->
    let f = eval lookup f in
    (match f with Fun (special,f,_) -> 
      if special then f lookup original_expression args
      else f lookup original_expression (List.map (eval lookup) args)
    | _ -> raise NotAFunction)
| Var (v,s) -> lookup v

let invarg () = raise InvalidArgs

let range _ _ = function
 | [Int start;Int stop] ->
        let l = ref [] in
        for i = stop downto start do l := Int i::!l done;
        List !l
| [Int start;Int stop;Int step] ->
        let l = ref [] in
        let i = ref start in
        while !i < stop do
          l := Int !i :: !l;
          i := !i + step
        done;
        List (List.rev !l)
| [Str start;Str stop] when String.length start = 1 &&
      String.length stop = 1 ->
        let start = Char.code start.[0] in
        let stop = Char.code stop.[0] in
        let l = ref [] in
        for i = stop downto start do 
          let c = String.make 1 (Char.chr i) in
          l := Str c::!l done;
        List !l
    | _ -> raise InvalidArgs

(* FIXME: add car, cons, cdr, let, other things to complete the 
 * lispy package.  Have some way of user-adding other functions.
 * If nothing else, be able to evaluate shell commands here? *)

let lambda lookup original_expression = function [List vars; e] ->
  Fun(false,(fun _ _ -> function args ->
    let env i =
      let rec sub = function
        [], [] -> lookup i
      | (Var(v,_))::vars, a::args -> if i=v then a else sub (vars,args)
      | _ -> invarg() in
      sub (vars,args) in
    eval env e),original_expression)
| _ -> invarg ()

let let_ = fun lookup _ -> function 
[List lets;e] ->
  let lets = List.map (function 
    List [Var (i,_);v] -> (i,eval lookup v)
  | _ -> invarg ()) lets in
  let env i = try List.assoc i lets with Not_found -> lookup i in
  eval env e
| _ -> invarg ()

let functions = [
(* bool is true if it's a special form (i.e. don't eval args before apply) *)
"quote",(true,(fun _ _ -> function [e] -> e | _ -> raise InvalidArgs));
"list",(false,(fun _ _ args -> List args));
"flatten",(false,(fun _ _ args -> List ( List.fold_right (fun elt acc -> 
  match elt with List l -> l@acc | x -> x::acc) args [])));
"decr",(false,(fun _ _ -> function [Int i] ->
        if i > 0 then List [Int (i-1)]
        else List [] 
  | [Int i;Int j] ->
        if i - j >= 0 then List [Int (i-j)]
        else List [] 
  | _ -> raise InvalidArgs));
"range",(false,range);
"nth",(false,fun _ _ -> function [List l;Int i] -> List.nth l i | _ -> invarg ());
"split",(false,(fun _ _ -> function [Str s] -> 
  List (List.map (fun s -> Str s) (Mystring.split s)) | _ -> invarg ()));
"concat",(false,(fun _ _ args ->
        Str (String.concat "" (List.map (function
           | Int i -> string_of_int i
           | Str s -> s
           | _ -> raise InvalidArgs) args))));
"lt",(false,(fun _ _ -> function [Int i;Int j] -> Bool (i<j) | _ -> invarg ()));
"gt",(false,(fun _ _ -> function [Int i;Int j] -> Bool (i>j) | _ -> invarg ()));
"lambda",(true,lambda);
"fun",(true,lambda);
"let",(true,let_);
"if",(true,fun lookup _ -> function [c;t;f] -> 
  (match eval lookup c with
  Bool b -> if b then eval lookup t else eval lookup f | _ -> invarg ())
  | _ -> invarg ());
"shell",(false,(fun _ _ -> function [Str s] -> Str (Util.getoutput s) 
                                  | _ -> invarg
()));
]

(** global variables that need to be entered in the outer system *)
let globals = List.map (function n,(s,f) -> n,(Fun(s,f,Var n))) functions

let rec compile intern_var = function
| (Int _ | Str _ | Bool _ ) as e ->  e
| Fun(s,f,e) -> Fun(s,f,compile intern_var e)
| Var s -> Var (intern_var s,s)
| List es -> List (List.map (compile intern_var) es)

let rec iter_vars f = function
  | Int _  | Str _ | Fun _ | Bool _ -> ()
  | Var v -> f v
  | List l -> List.iter (iter_vars f) l

