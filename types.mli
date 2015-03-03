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
type io = Input | Output
(* initially 'a is string, but it's later interned to (int * string) *)
type 'a expr = Int of int | Str of string | Bool of bool
| List of 'a expr list
| Var of 'a
(** Fun(s,f,e) represents a function expression.
   s is a boolean, true if the function is a special form, i.e. it's applied
     to unevaluated arguments.
   f is the function itself.
     f env o args returns the result of evaluating f on the list of arguments
     args in the environment env (env is a map from a variable index to
     an exprssion).  o is the unevaluated application expression, probably
     (f args)
   e is an expression which evaluates to this expression - it's used to
     represent this expression as a string *)
| Fun of bool * ((int -> (int * string) expr) -> (int * string) expr ->
    (int * string) expr list ->
    (int * string) expr) * 'a expr

type splat = Splat | Flat

(* kvs will either be the initial string to expression association list, but
 * it's later compiled into an expression array *)

type suffix = string option

type 'a node = {kvs: 'a; io: io; suffix: suffix}

type rawkvs = (string * string expr * splat) list
type skvs = ((int * string) expr * splat) option array
type kvs = (int * string) expr option array

type ('a,'c) topexpr = Expr of 'c | Node of 'a node

type ('a,'c) word = ('a,'c) topexpr list
type ('a,'c) sentence = ('a,'c) word list

type raw_sentence = (rawkvs, string expr * splat) sentence

type group = Rule of string * raw_sentence * int 
| Def of string * raw_sentence * int

type rule = {
  line: int; (* line number of rule *)
  comment: string;
  command: (skvs,(int * string) expr * splat) sentence;
}

(* interpolated rule *)
type irule = {
  i_kvs: kvs;
  i_command: (kvs, string) topexpr list;
  i_rule: rule;
  i_outputs: kvs node list;
}

(** interpolated rules with filenames *)

type firule = {
  irule: irule;
  descr: string;
  dir: string;
  outputfns: string list;
  mutable last_modification: 
    [`Timestamp of float * string | `InputRebuilt of string]; 
  fi_command: string; (** actual command string to execute *)
}

(* a rule producing a specific output *)
type orule = {
  output : kvs node;
  rule: rule;
}

(* dictionary for generating key, value labels *)
type dict = {
  rglobals: ((int * string) expr, string) Hashtbl.t;
  m: (string * int, string * (int * string) expr) Hashtbl.t;
  dictf: out_channel option;
  max_label_len: int;
  keys: string array;
  key_order: int list;
  base: string;
  includekeys: bool;
}

