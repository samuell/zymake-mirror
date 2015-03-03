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
(* A kvsmap is a map (dictionary, associative array) 
   from a set of (subkey, subvalue) pairs (a "superkey" or "kvs"; key-value set)
   to a supervalue.

   This is a mutable (imperative, destructive-update) data structure.
  
   The special property of a kvsmap is that looking up a superkey k will find
   a map binding k',v if k' is a subset of k (not just if k'=k as in a normal 
   map).

   Assumptions: 
     A superkey only contains one of each subkey
     All superkeys in a given map will have one of a small (1 or 2) number of
     sets of subkeys.
     You can compare subkeys with =
 *)
type ('subkey, 'subvalue, 'supervalue) cell =
  {subkeys: 'subkey list; m: ('subvalue list, 'supervalue) Hashtbl.t }
(* AF: [cell0; cell1; cell1...] represents the union of the maps represented
   by each cell.

   a cell {[k0,k1, ...,kn], {(v0,v1,..vn)=>V, (v'0,v'1, ...v'n)=>V',...}} 
   represents the map from
   {k0=>v0, k1=>v1, ... kn=>vn} => V,
   {k0=>v'0,k1=>v'1, ... kn => v'n} => V',
   ...
*)
type ('subkey, 'subvalue, 'supervalue) t =
  ('subkey, 'subvalue, 'supervalue) cell list ref

module Kvs = struct
  (* do I want this set implementation? does it matter? *)
  (* RI: subkeys are in sorted order *)
  type ('subkey, 'subvalue) t = ('subkey * 'subvalue) list
  (* = keys of kvs in key-sorted order *)
  let get_subkeys kvs = List.map fst kvs
  (* = values of kvs in key-sorted order *)
  let get_subvalues kvs = List.map snd kvs
  (* = in order, values corresponding to subkeys in kvs *)
  let project kvs subkeys = (* this is inefficient FIXME *)
    List.map (fun subkey -> List.assoc subkey kvs) subkeys
end

let create () = ref []

let length m = List.fold_left (fun tot cell -> tot + Hashtbl.length cell.m) 0 !m

(* folds f over all supervalues in the map m *)
let foldv f m acc = 
  List.fold_left (fun acc cell ->
    Hashtbl.fold (fun k v acc -> f acc v) cell.m acc) acc !m

let add superkey supervalue m =
  let subkeys = Kvs.get_subkeys superkey in
  let subvalues = Kvs.get_subvalues superkey in
  let rec loop = function
  | {subkeys=subkeys'; m=subm} :: rest ->
      if subkeys'=subkeys (* FIXME: is this the right comparison func? *) then (
        Hashtbl.add subm subvalues supervalue; 
      ) else
        loop rest
  | [] -> 
      let subm = Hashtbl.create 10 in
      Hashtbl.add subm subvalues supervalue;
      m:={subkeys=subkeys; m=subm}::!m in
  loop !m

let find superkey m =
  let rec loop = function
  | {subkeys=subkeys; m=subm} :: rest ->
      (try 
        let subvalues = Kvs.project superkey subkeys in
        Hashtbl.find subm subvalues 
      with Not_found ->
        loop rest)
  | [] -> raise Not_found in
  loop !m

open Printf
(** testing code **)

let test () = begin
  let string_of_submap s = "{" ^ (String.concat " " (List.map (fun (k,v) ->
    sprintf "%d => %s," k v) s)) ^ "}" in
  let try_lookup m s =
    try 
      let _ = find s m in true
    with Not_found -> false in
  let test_case f arg expected_output = 
    if f arg = expected_output then print_string "Ok\n"
    else printf "Failed on input %s\n" (string_of_submap arg) in
  let superkey1 = [(0,"zero");(3,"three")] in
  let superkey2 = [(2,"two")] in
  let m = create () in
  add superkey1 "node0" m;
  add superkey2 "node1" m;
  (* look up the exact superkeys we put in *)
  test_case (try_lookup m) superkey1 true;
  test_case (try_lookup m) superkey2 true;
  (* look up a superset of the first superkey we put in *)
  test_case (try_lookup m) [(0,"zero");(1,"one");(3,"three")] true;
  (* look up a subset of the first superkey we put in *)
  test_case (try_lookup m) [(0,"zero")] false;
  (* look up a map with the same subkeys but different subvalues *)
  test_case (try_lookup m) [(0,"zilch");(3,"three")] false;
  (* look up a map with a totally different superkey*)
  test_case (try_lookup m) [(4,"four")] false;
  (* look up an empty map*)
  test_case (try_lookup m) [] false;
end
