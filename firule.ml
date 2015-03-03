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
* I need a better name than "firule", but what this module does is prepare the
* dependency dag to actually be run - principally by figuring out the filenames
* for all the nodes, and also determining whether each node actually needs to
* be executed.
*)

open Types
open Util
open Printf

let label_ok = function (* note: this should match the id pattern in lexer.mll *)
| 'a' .. 'z' | '0' .. '9' | 'A' .. 'Z' | '_' | '-' -> true  
| _ -> false

(** list_trunc n l = the first n elements of l *)
let rec list_trunc n = function [] -> [] | h :: t ->
  if n <= 0 then [] else h :: (list_trunc (n-1) t)

let get_label s max_label_len = 
  let ok_chars = List.filter label_ok (explode_string s) in
  match implode_string (list_trunc max_label_len ok_chars) with 
  | "" -> "-"
  | s -> s

(** get_kv_label dict (k,v) - return a globally unique short string
  * that can be used as a component of a filename which maps to (k,v).
  * Effect: may write out to dictionary file the newly assigned mapping *)
let get_kv_label dict (k,v) =
  (* if v is the value of a global variable, use the name of that global 
   * variable as the label; otherwise convert v to a string *)
  let l =
    try Hashtbl.find dict.rglobals v
    with Not_found -> Eval.string_of_expr v in
  (* remove non-filesystem-safe characters from l, truncate to max_label_len *)
  let l = get_label l dict.max_label_len in
  let rec getindex i = 
    try
      let kv' = Hashtbl.find dict.m (l,i) in
      if kv' = (k,v) then i
      else getindex (i+1)
    with Not_found ->
      Hashtbl.add dict.m (l,i) (k,v);
      (match dict.dictf with None -> () |
       Some f -> fprintf f "%s %d %s %s\n%!" l i k (Eval.repr dict.keys v));
      i in
  let i = getindex 0 in
  if i = 0 then l else sprintf "%s~%d" l i

(** print a .-separated list of filename-happy labels corresponding to the
  * kvs to the buffer b.
  * Effect: may write out to dictionary file the newly assigned mapping *)
let addkvs dict b kvs =
  List.iter (fun i -> match kvs.(i) with None -> ()
  | Some v ->
      Buffer.add_char b '.';
      if dict.includekeys then bprintf b "%s=" dict.keys.(i);
      Buffer.add_string b (get_kv_label dict (dict.keys.(i),v));
  ) dict.key_order

let (//) a b = Filename.concat a b

(** return a string filename corresponding to a node
  * Effect: may write out to dictionary file *)
let getnodename dict n = 
  let b = Buffer.create 20 in
  Buffer.add_string b (dict.base // "o");
  addkvs dict b n.kvs;
  (match n.suffix with None -> () | Some s -> bprintf b ".%s" s);
  Buffer.contents b

(** return a string directory name corresponding to an interpolated rule
  * Effect: may write out to dictionary file *)
let getruledir dict irule =
  let b = Buffer.create 20 in
  bprintf b "%s%d" (dict.base // ".zymake" // "rule-") irule.i_rule.line;
  addkvs dict b irule.i_kvs;
  Buffer.contents b

(* create directory "dir" with open permissions, no-oping if already exists *)
let safe_mkdir dir =
  try Unix.mkdir dir 0o777 
  with Unix.Unix_error(Unix.EEXIST,_,_) -> ()

(** return a string corresponding to an interpolated command
  * Effect: may write out to dictionary file *)
let string_of_command dict command = 
  String.concat "" (List.map (function Expr s -> s
  | Node n -> getnodename dict n) command)

(* build time logic *)

let earlier_output f s = match f,s with
| `NoOutputs,other | other,`NoOutputs -> other
| `OutputMissing _ as missing,_ | _,(`OutputMissing _ as missing) -> missing
| `Timestamp (t0,f0), `Timestamp (t1,f1) -> 
  `Timestamp (if t0 < t1 then (t0,f0) else (t1,f1))

let later_input f s = match f,s with
| `NoInputs,other | other,`NoInputs -> other 
| `InputRebuilt _ as rebuilt,_ | _,(`InputRebuilt _ as rebuilt) -> rebuilt
| `Timestamp (t0,f0), `Timestamp (t1,f1) -> 
  `Timestamp (if t0 > t1 then (t0,f0) else (t1,f1))

(* returns None if it is not necessary to rebuild the current node,
 * or Some explanation if it is *)
let rebuild inputs outputs =
  let latest_input = List.fold_left later_input `NoInputs 
    (inputs :> [ `InputRebuilt of string | `NoInputs | `Timestamp of float * string ] list)  in
  let earliest_output = List.fold_left earlier_output `NoOutputs outputs in
  match (latest_input, earliest_output) with 
  | `InputRebuilt f, _ -> Some (sprintf "input %s was rebuilt" f)
  | _ , `OutputMissing f -> Some (sprintf "output %s is missing" f)
  | `Timestamp (latest_input_time, latest_input_file), 
    `Timestamp (earliest_output_time, earliest_output_file)
    when latest_input_time > earliest_output_time ->
      Some(sprintf "input file %s is newer (%s) than output file %s (%s)"
       latest_input_file (strtime latest_input_time)
       earliest_output_file (strtime earliest_output_time))
  | _ -> None

(** Returns a timestamp for the current node
  * `InputRebuilt d if the node needs to be rebuilt
  * or `Timestamp (t,f) otherwise, where f has the latest filesystem timestamp t
  * for any of the files created by this node
  *)
let should_build descr outputfns parents =
  if !verbose > 1 then eprintf "trying to build %s from (%s)\n%!"
    descr
    (String.concat " " (List.map (fun n -> n.descr) parents));

  let output_times = List.map (fun fn -> get_time fn) outputfns in
  let input_times = List.map (fun n -> n.last_modification) parents in
  let why_rebuild = rebuild input_times output_times in
  match why_rebuild with 
  | Some why ->
    if !verbose > 1 then begin
      eprintf "building %s because %s\n%!" descr why;
    end;
    `InputRebuilt descr
  | None ->
    let output_times = List.map (function `Timestamp t -> t
    | _ -> invalid_arg "output_times") output_times in
    let latest_time = List.fold_left (fun (t0,f0) (t1,f1) -> if t0 > t1 then
      (t0,f0) else (t1,f1)) (neg_infinity,"") output_times in
    `Timestamp latest_time

(* return how long it took this node to run, or None if the run-time
 * is unavailable *)
let run_time firule = 
  try
    let f = open_in (firule.dir // "time") in
    let r = Scanf.fscanf f "%d" (fun i -> i) in
    close_in f;
    Some r
  with _ ->
    None

(** public functions *)

let firule_of_irule dict irule parents = 
  let outputfns = List.map (getnodename dict) irule.i_outputs in
  let command = string_of_command dict irule.i_command in
  let dir = getruledir dict irule in
  (match dict.dictf with None -> () | Some _ -> safe_mkdir dir);
  let descr = match outputfns with [fn] -> fn
    | _ ->
    sprintf "(Rule at line %d; keys {%s})" irule.i_rule.line 
    (Repr.repr_kvs dict.keys irule.i_kvs) in
  {
    irule = irule;
    outputfns = outputfns;
    dir = dir;
    descr = descr;
    last_modification = should_build descr outputfns parents;
    fi_command = command;
  }

(* FIXME; specify the directory structure somewhere ONCE *)
let make_dictfn fn = "o" // (Filename.basename fn) // ".zymake" //"dict"

let check_long_computations firules long interactive =
  let long_files = Topo.fold (fun l n -> 
    let firule = Topo.get_data n in
    match firule.last_modification with
    | `InputRebuilt _ ->
      (match run_time firule with
        | Some t when t >= long -> 
            (List.map (fun fn -> fn,t) firule.outputfns) @ l
        | _ -> l)
    | _ -> l
  ) [] firules in

  match long_files with
  | [] -> ()
  | filenames -> 
      if interactive then begin
        eprintf 
"Executing this run will overwrite the following files which took at least
%d seconds to execute.  Re-run with -f flag to force execution.\n" long;
        List.iter (fun (fn,t) -> eprintf "%s %d seconds\n" fn t) filenames; 
        exit 1
      end

(** 
* returns: a dictionary used for mapping nodes to actual filenames
* Effects: ensures that the relevant o/ directories exist (if actually running)
*          adds to the dictionary keys for all necessary k,vs (if running)  *)
let make_dict keys makefilefn includekeys empty_locals dorun 
  max_label_len dictm globals dictfn =

  (* if we're actually building, then make sure the o/ directory is there *)
  let m = Filename.basename makefilefn in
  let o_m = "o" // m in
  if dorun then begin
    safe_mkdir "o"; 
    safe_mkdir o_m;
    safe_mkdir (o_m // ".zymake")
  end;

  let key_order = range (Array.length empty_locals) in
  let key_order = List.sort (fun a b -> compare keys.(a) keys.(b)) key_order in

  let rglobals = Hashtbl.create (Array.length globals) in
  Array.iteri (fun i v ->
    match v with None -> () | Some v -> Hashtbl.add rglobals v keys.(i) 
  ) globals;

  {
    dictf=if dorun then Some (open_out_gen [Open_append;Open_creat] 0o666 dictfn) else None;
    m=dictm;
    keys=keys;
    rglobals=rglobals;
    key_order=key_order;
    base=o_m;
    Types.max_label_len=max_label_len;
    Types.includekeys=includekeys;
  }
