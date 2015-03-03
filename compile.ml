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
(** 
* take the parsed makefile and do the following things:
  * create a map from suffix -> list of (output-node, rule) pairs
  * collect the list of variable names (keys and globals) used
  * collect a map from variable index to value (expression)
  * find the roots (rules without outputs)
  * replace variable strings by their indices.
  
  * public functions: interpolate and compile_makefile
  
  * interpolate interpolates a top-expression list in context
    (i.e. given a particular output node it's trying to make)
*)
(** maybe this module should be topexpr.ml ? *)
(* note: all Eval.evaluations happen in this module *)
(* FIXME: this module is a total mess.... sorry! *)
open Types
open Printf
open Util

exception InvalidSplat
exception NodeInDef
exception UndefinedVariable of int
exception UndeclaredVariable of string

let string_of_exn keys = function
| Eval.NotAFunction -> Some "first element of list is not a function"
| Eval.InvalidArgs -> Some "invalid arguments to function"
| Eval.EvalEmptyList -> Some "tried to evaluate empty list"
| UndeclaredVariable s -> Some ("variable " ^ s ^ " undeclared")
| NodeInDef -> Some "definition not allowed" 
| InvalidSplat -> Some "invalid splat (argument of * must be a list)"
| UndefinedVariable v -> Some ("variable " ^ keys.(v) ^ " undefined")
| _ -> None

let rec list_product = function [] -> [[]]
| h::t ->
  let t = list_product t in
  List.fold_right (fun rest acc ->
    (List.map (fun v -> v::rest) h) @ acc
  ) t []

type definitions = {
  locals: kvs;
  globals: kvs;
  used_locals: bool array;
}

let lookup definitions name = 
  let local = 
    if name >= Array.length definitions.locals then None
  else 
    definitions.locals.(name) in
  match local with Some v ->
    definitions.used_locals.(name) <- true;
    v
  | None -> 
  match definitions.globals.(name) with Some v -> v
  | None ->
    raise (UndefinedVariable name)

let eval_node env n =
  let kvs = Array.to_list n.kvs in
  let kvs = List.map (function None -> [None]
  | Some (e,splat) -> match Eval.eval env e,splat with
              | e,Flat -> [Some e]
              | (List l),Splat -> List.map (fun e -> Some e) l
              | _ -> raise InvalidSplat) kvs in
  let kvses = list_product kvs in
  List.map (fun kvs -> {n with kvs=Array.of_list kvs}) kvses

let eval_topexpr env = function
| Expr (e,splat) -> (match Eval.eval env e,splat with
| (e,Flat) -> [Expr e]
| ((List l),Splat) -> List.map (fun e -> Expr e) l
| _ -> raise InvalidSplat)
| Node n -> List.map (fun n -> Node n) (eval_node env n)

let eval_word env word = list_product (List.map (eval_topexpr env) word)

let eval_sentence env sentence =
  List.fold_right (fun word acc -> (eval_word env word) @ acc) sentence []

let expr_of_word l =
  let l = List.map (function 
    Node _ -> raise NodeInDef
  | Expr e -> e) l in
  match l with
  | [h] -> h
  | _ -> Str (String.concat "" (List.map Eval.string_of_expr l))

(* evaluate a sentence and return a single expression *)
let expr_of_sentence globals locals l = 
  let defs = {globals = globals; locals= locals; used_locals = Array.make
         (Array.length locals) false} in
  let l = eval_sentence (lookup defs) l in
  match List.map expr_of_word l with
  | [e] -> e
  | l -> List l

(* evaluate a sentence and return all the output nodes in it *)
let outputs_of_sentence globals locals l =
  let defs = {globals = globals; locals = locals; used_locals =Array.make
  (Array.length locals) false} in
  let outputs = List.fold_right (
    List.fold_right (fun topexpr nodes ->
      match topexpr with
      | Node n when n.io=Output -> (eval_node (lookup defs) n) @ nodes
      | _ -> nodes
    ) 
  ) l [] in
  Util.uniq_struct outputs

(* take a sentence and return a list of either nodes or strings *)
let interpolate globals locals l = 
  let defs = {globals = globals; locals = locals; used_locals =Array.make 
  (Array.length locals) false } in
  let s = eval_sentence (lookup defs) l in
  let l = List.fold_right (fun word exprs ->
    (List.fold_right (fun topexpr exprs ->
      (match topexpr with
      | Expr e -> Expr (Eval.string_of_expr e)
      | Node _ as n -> n) :: exprs
    ) word (match exprs with [] -> [] | l -> Expr " "::l))
  ) s [] in
  l, defs.used_locals

let compile_topexpr intern_var empty_locals = function
| Node n ->
    let kvs = Array.make (Array.length empty_locals) None in
    List.iter (fun (k,v,splat) -> 
      kvs.(intern_var k) <- Some (Eval.compile intern_var v,splat);
    ) n.kvs;
    Node {n with kvs=kvs}
| Expr (e,splat) -> Expr (Eval.compile intern_var e,splat)

let compile_makefile groups dictionary query =

  let rkeys = Hashtbl.create 10 in
  let add_var n = 
    if not (Hashtbl.mem rkeys n) then 
      Hashtbl.add rkeys n (Hashtbl.length rkeys) in

  (* first, collect all the keys mentioned anywhere *)
  let add_sentence_keys s =
    List.iter (fun l ->
      List.iter (function Expr _ -> () | Node n -> 
        List.iter (fun (k,_,_) -> add_var k) n.kvs
      ) l
    ) s in

  List.iter (function Rule (_,s,_) | Def(_,s,_) -> add_sentence_keys s) groups;

  List.iter (fun (l,i,k,v) -> add_var k) dictionary;

  add_sentence_keys query;

  (* used in several places, this is an empty key-value set *)
  let empty_locals = Array.make (Hashtbl.length rkeys) None in

  (* now, collect the global variable names *)
  List.iter (fun (n,_) -> add_var n) Eval.globals;
  
  List.iter (function Rule _ -> () | Def (n,_,_) -> add_var n) groups;

  (* now, collect all variables used in any expression *)
  (* this shouldn't add anything, but you might have locally bound vars
     (like in a lambda) that don't correspond to a key or a global *)
  let add_sentence_vars s =
    List.iter (fun l ->
      List.iter (function 
      | Expr (e,_) -> 
        Eval.iter_vars add_var e
      | Node n -> 
        List.iter (fun (_,e,_) -> Eval.iter_vars add_var e) n.kvs;
      ) l
    ) s in

  List.iter (function Rule (_,s,_) | Def (_,s,_) -> add_sentence_vars s) groups;
  add_sentence_vars query;
  List.iter(fun (_,_,_,e) -> Eval.iter_vars add_var e) dictionary;

  (* if UndeclaredVariable is raised, I screwed up somewhere ... *)
  let intern_var s = 
    try Hashtbl.find rkeys s with Not_found -> raise (UndeclaredVariable s) in

  (* create the int -> string map *)
  let keys = Array.make (Hashtbl.length rkeys) "" in
  Hashtbl.iter (fun k i -> keys.(i) <- k) rkeys;

  (* define global variables *)
  let globals = Array.make (Array.length keys) None in
  List.iter (fun (n,e) -> 
    try
      globals.(intern_var n) <- Some (Eval.compile intern_var e)
    with UndeclaredVariable v ->
      eprintf "Error compiling pre-defined global %s; %s undeclared\n" n v;
      exit 1
  ) Eval.globals;

  List.iter (function Rule _ -> () | Def (n,s,ln) ->
    try
      let s = List.map (List.map (compile_topexpr intern_var empty_locals)) s in
      globals.(intern_var n) <- Some (expr_of_sentence globals empty_locals s)
    with e ->
      eprintf "Error compiling definition on line %d: %!" ln;
      let msg = match string_of_exn keys e with
      | Some s -> s | None -> raise e in
      eprintf "%s\n" msg;
      exit 1;

  ) groups;

  (* debug output... *)
  if !verbose > 1 then begin
    Array.iteri (fun i -> function None -> () | Some v ->
      printf "%s = %s\n" (keys.(i)) (Eval.repr keys v)
    ) globals;
  end;

  let rules = List.fold_left (fun acc -> function
    Rule (comment,command,line) ->
      let command = try
        List.map (List.map (compile_topexpr intern_var empty_locals)) command 
      with UndeclaredVariable v ->
        eprintf "Error compiling rule at line %d: undeclared variable %s\n" 
        line v;
        exit 1 in
      {comment = comment; command = command; line=line}::acc
  | _ -> acc) [] groups in
  let orules = Hashtbl.create 50 in

  let roots = List.fold_left (fun roots rule ->
    let outputs = try
      outputs_of_sentence globals empty_locals rule.command
    with e ->
      eprintf "Error in determining outputs of rule at line %d: %!" rule.line;
      let msg = match string_of_exn keys e with
      | Some s -> s | None -> raise e in
      eprintf "%s\n" msg;
      exit 1
    in
    List.iter (fun output ->
      let key = output.suffix in
      let oldval = try Hashtbl.find orules key with Not_found -> [] in
      Hashtbl.replace orules key ({output=output; rule=rule}::oldval);
    ) outputs;
    if outputs=[] then rule::roots else roots
  ) [] rules in

  let dictm = Hashtbl.create (List.length dictionary) in

  List.iter (fun (l,i,k,v) ->
    try
      Hashtbl.add dictm (l,i) (k,Eval.compile intern_var v)
    with UndeclaredVariable v' ->
      eprintf "Error compiling dictionary entry (%s,%d,%s): undeclared\
      variable %s\n" l i k v';
      exit 1;
  ) dictionary;

  let query = try
    let query = 
      List.map (List.map (compile_topexpr intern_var empty_locals)) query in
    let query,_ = interpolate globals empty_locals query in
    query
  with e ->
    eprintf "Error compiling query string: %!";
    let msg = match string_of_exn keys e with
    | Some s -> s | None -> raise e in
    eprintf "%s\n" msg;
    exit 1 in

  keys, empty_locals, globals, orules, roots, dictm, query
