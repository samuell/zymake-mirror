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
(* 
This file contains the interesting code for zymake: it exports
construct_dependency_dag keys empty_locals globals orules roots 

which figures out, given the root rules, the whole dag of actual file nodes
that has to be built to support those roots.  It does the pushdown to figure
out which nodes can be shared, etc.

*)

open Types
open Util
open Printf

(** do all of the rule's output's keys exist in my node with the same value? *)
let node_match node rule =
  let l = Array.length node.kvs in
  let rec sub i =
    if i = l then true
    else (rule.output.kvs.(i) = None || rule.output.kvs.(i) = node.kvs.(i))
    && sub (i+1) in
  sub 0 && node.suffix = rule.output.suffix

(** add additional to original, but do not overwrite bindings 
    see afg/scripts/exp3 for an example of where this is important: true.true *)
let combine_kvs original additional =
  Array.mapi (fun i -> function Some _ as v -> v
  | None -> additional.(i)) original

let expand_outputs locals l =
  List.map (function 
  | (Node ({io=Output} as n)) -> Node {n with kvs=combine_kvs n.kvs locals}
  | x -> x) l

exception NoMatch of kvs node
exception TooManyMatches of kvs node * orule list

let get_outputs l = 
  List.fold_left (fun acc -> function
    Node({io=Output} as  n) -> n::acc
  | _ -> acc) [] l

let getrule rules n = 
  let rules = try
    Hashtbl.find rules n.suffix
  with Not_found -> raise (NoMatch n) in
  let matches = List.filter (node_match n) rules in
  match matches with
  | [rule] -> rule
  | [] -> raise (NoMatch n)
  | l -> raise (TooManyMatches(n,l))

type ('a,'b) context = {
  keys: string array;
  globals: kvs;
  rules: (suffix, orule list) Hashtbl.t;
  rule_to_kvsmap: ('a,(int, (int * string) expr, int * (int list * irule)) Kvsmap.t) Hashtbl.t;
  mutable num_dagnodes: int;
}

(* FIXME: this function shouldn't exist, I should modify kvsmap to use a
   different map type or actually use a sorted association list elsewhere *)
let list_of_loption a =
  let ret = ref [] in
  for i = 0 to Array.length a - 1 do
    match a.(i) with None -> ()
    | Some v -> ret := (i,v)::!ret
  done;
  !ret

let rec interpolate c rule locals = 
  (*
  if !verbose > 2 then begin
    printf "interpolate_rule called; rule %d\n" rule.line;
    printf "%s\n" (string_of_topexpr_list c.keys rule.command);
    Array.iteri (fun i v ->
      match v with None -> () | Some v ->
      printf " %s = %s\n" c.keys.(i) (Eval.repr c.keys v)
    ) locals;
    printf "\n";
  end;
*)
  let command, keys_used = 
    try Compile.interpolate c.globals locals rule.command 
    with e ->
      eprintf "Error interpolating rule at line %d\n%!" rule.line;
      match Compile.string_of_exn c.keys e with 
      | Some msg -> eprintf "%s\n" msg; exit 1 
      | None -> raise e in

  let dependencies = ref [] in

  let command = List.fold_right (fun x command -> match x with
  | Node ({io=Input} as n) -> 
      let locals = combine_kvs n.kvs locals in
      let orule = try
        getrule c.rules {n with kvs=locals} 
      with NoMatch n -> begin
        eprintf "Error: in rule at line %d; cannot find rule to create input \
        node: %s\n" rule.line (Repr.repr_node c.keys n);
        exit 1 end 
      | TooManyMatches (n,l) -> begin
        eprintf "Error: in rule at line %d; it's ambiguous how to create input \
        node %s\n" rule.line (Repr.repr_node c.keys n);
        List.iter (fun orule -> 
            eprintf "  Could be rule at line %d, producing output %s\n"
            orule.rule.line (Repr.repr_node c.keys orule.output)
        ) l;
        exit 1 end 
      in
      let irule_index, (_, irule) = gimme_dagnode c orule.rule locals in
      dependencies := irule_index :: !dependencies;
      let kvs = combine_kvs orule.output.kvs irule.i_kvs in
      for i = 0 to Array.length kvs - 1 do
      (* a key only counts as used by an input if it wasn't directly
         specified in this rule (i.e. in the original node) *)
        if kvs.(i) <> None && n.kvs.(i) = None then
          keys_used.(i) <- true
      done;
      Node {n with kvs = kvs} :: command
  | x -> x::command) command [] in
  let dependencies = uniq !dependencies in
  command, keys_used, dependencies

and gimme_dagnode c rule locals = 
    let kvsmap = 
      try Hashtbl.find c.rule_to_kvsmap rule 
      with Not_found -> 
        let m = Kvsmap.create () in 
        Hashtbl.add c.rule_to_kvsmap rule m;
        m in
    try 
      Kvsmap.find (list_of_loption locals) kvsmap
    with Not_found -> begin
      let command, keys_used, dependencies = interpolate c rule locals in

      let locals = Array.mapi (fun i v -> if keys_used.(i) then v else None)
      locals in

      (* add only these fields of n to output nodes *)
      let command = expand_outputs locals command in

      (*
      if !verbose > 2 then 
        printf "interpolate_rule finished; %s\n" (string_of_ilist c.keys command);
      *)

(*      printf "add irule for %d (%s)\n" rule.line (repr_kvs keys locals); *)
      let irule = {
        i_kvs=locals;
        i_command=command;
        i_rule=rule;
        i_outputs=get_outputs command;
      } in
      let index = c.num_dagnodes in
      c.num_dagnodes <- c.num_dagnodes+1;
      Kvsmap.add (list_of_loption locals) (index, (dependencies, irule)) kvsmap;
      index, (dependencies, irule)
    end

let array_of_assoc_list = function
  [] -> [||]
| (_,v) :: tl ->
    let a = Array.create (List.length tl + 1) v in
    List.iter (fun (i,v) -> a.(i) <- v) tl;
    a

(* fold a function over just the values in a hashtable *)
let hashtbl_foldv f = Hashtbl.fold (fun k v a -> f v a)

let construct_dependency_dag keys empty_locals globals orules roots =
  let c = {
    globals=globals;
    rules=orules;
    keys=keys;
    rule_to_kvsmap = Hashtbl.create 10;
    num_dagnodes = 0;
  } in

  (* create a dagnode for each root *)
  List.iter (fun rule -> ignore (gimme_dagnode c rule empty_locals)) roots;

  (* return an array mapping the dagnode indices to each dagnode *)
  let ret = hashtbl_foldv (Kvsmap.foldv (fun a i -> i::a)) c.rule_to_kvsmap []in
  let a = array_of_assoc_list ret in
  let r = Topo.new_graph (fun i -> fst a.(i)) (fun i -> snd a.(i)) (Array.length
  a) in
  Topo.reverse r

