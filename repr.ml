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

let buffer_add_kvs keys b kvs =
  Array.iteri (fun i -> function
    None -> () | Some v -> bprintf b "%s=" keys.(i); Eval._reprspc keys b v
  ) kvs

let buffer_add_skvs keys b kvs =
  Array.iteri (fun i -> function
    None -> () | Some (v,s) -> 
      bprintf b "%s=" keys.(i); 
      (match s with Flat -> () | Splat -> Buffer.add_char b '*');
      Eval._reprspc keys b v
  ) kvs

let buffer_add_node keys b n =
  Buffer.add_char b '{';
  Buffer.add_char b (match n.io with Input -> '<' | Output -> '>');
  buffer_add_kvs keys b n.kvs;
  Buffer.add_char b '}';
  (match n.suffix with Some s -> bprintf b ".%s" s | None -> ())

let buffer_add_snode keys b n =
  Buffer.add_char b '{';
  Buffer.add_char b (match n.io with Input -> '<' | Output -> '>');
  buffer_add_skvs keys b n.kvs;
  Buffer.add_char b '}';
  (match n.suffix with Some s -> bprintf b ".%s" s | None -> ())

let repr_kvs keys kvs = repr (buffer_add_kvs keys) kvs
let repr_node keys n = repr (buffer_add_node keys) n

let string_of_ilist keys l = 
  let b = Buffer.create 10 in
  List.iter (function
    Expr s -> Buffer.add_string b s
  | Node n -> buffer_add_node keys b n) l;
  Buffer.contents b

(* this is for (skvs,int lexpr * splat) topexpr *)
let string_of_topexpr_list keys l = 
  let b = Buffer.create 10 in
  List.iter (fun word ->
    List.iter (function
    | Expr (e,_) -> Eval._repr keys b e;
    | Node n -> buffer_add_snode keys b n
    ) word;
    Buffer.add_char b ' ';
  ) l;
  Buffer.contents b
