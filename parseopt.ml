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
  Copyright (c) 2006, Eric Breck
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1 Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

  2 Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  3 The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
open Printf

type keyspec = Short | Long1 | Long2
type parsetype = Gnu | Caml

let set = fun _ -> true
let clear = fun _ -> true
let store = fun v _ -> v

type 'a option_parameters = {
  default: 'a;
  specs: (string * string list * string * string option * 'a action) list;
  to_string: 'a -> string;

  (* these are initialized from option_defaults, below *)
  keyspecs: keyspec list ;
  usage_prefix: string ;
  usage_suffix: string ;
  parsetype: parsetype;
  is_option: string -> bool ;
  dashdash: bool;
  helpkeys: string list;
  helpfieldwidths: int * int * int;
  keepgoing: bool;
}

and 'a action_parameters = {
  options: 'a;
  args: string list;
  option_parameters: 'a option_parameters;
}

and 'a action =
  Action of ('a action_parameters -> 'a)
| Conversion of ('a action_parameters -> string -> 'a)
| Handler of ('a action_parameters -> 'a * string list)

let option_defaults = {
  (* placeholders *)
  default = ();
  specs= [];
  to_string = (fun () -> "");

  (* parameters that can be changed by overriding these fields *)
  helpkeys = ["-h";"-help";"--help"; "-?"];
  keyspecs = [Short; Long1];
  usage_prefix = "";
  usage_suffix = "";
  parsetype = Caml;
  is_option = (fun s -> String.length s > 1 && s.[0]='-') (* >1 so can use '-' *);
  dashdash = true;
  helpfieldwidths = 15,10,52;
  keepgoing = true;
}

exception MissingArgument of string
exception UnknownOption of string
exception ConversionFailure of string * string * string

(* split string s at the last space before position len *)
let splitspc (s,len) =
  if String.length s <= len then s,""
  else
    let p,n =
      try
        let p = String.rindex_from s len ' ' in
        p,p+1
      with Not_found -> len,len (* give up and break at point *) in
    String.sub s 0 p,String.sub s n (String.length s - n)

(* line-wrap cols; format is [field,fieldwidth;field,fieldwidth;...] *)
let rec print_row cols =    
  let splits = List.map splitspc cols in
  List.iter2 (fun (cur,_) (_,len) -> Printf.printf "%-*s " len cur) splits cols;
  print_string "\n";
  if List.exists (fun (_,rest) -> rest <> "") splits then
    print_row (List.map2 (fun (_,rest) (_,len) -> rest,len) splits cols)

let getspecs option_parameters =
  (* set the keys for each option, using Op.keyspecs if keys not provided *)
  let specs = List.map (function
  | (fieldname,[],help,arg,action) ->
      (List.fold_right (fun key acc -> match key with
      | Short -> (sprintf "-%c" fieldname.[0])::acc
      | Long1 -> ("-" ^ fieldname)::acc
      | Long2 -> ("--" ^ fieldname)::acc) option_parameters.keyspecs []),help,arg,action
  | (_,explicit_keys,help,arg,action) -> explicit_keys,help,arg,action) 
    option_parameters.specs in

  (* optionally, add -- option to end further options *)
  let specs = if not option_parameters.dashdash then specs else
    specs @ 
      [["--"],"end options", None, Handler (fun ap -> ap.options,ap.args)] in
  specs

let gethelps specs option_parameters =
  let helps = List.map (fun (keys,help,arg,_) -> keys,help,arg) specs in

  let helps = helps @ [option_parameters.helpkeys,"display this help",None] in
  helps

(** print a help message containing all options *)
let help option_parameters = 
  let specs = getspecs option_parameters in
  let helps = gethelps specs option_parameters in
  let specs = List.sort (fun (a,_,_) (b,_,_) -> compare a b) helps in
  let kw,aw,hw = option_parameters.helpfieldwidths in
  List.iter (fun (keys,help,arg) ->
    print_row [(String.concat ", " keys),kw;(match arg with None -> "" | Some
    arg -> arg),aw; help,hw];
  ) specs

let getactions specs option_parameters =
  let actions = List.fold_left (fun acc (keys,_,_,action) ->
     List.fold_left (fun acc key -> (key,action)::acc) acc keys) [] specs in

  let actions = actions @ (List.map (fun key -> 
    key, Handler (fun _ -> help option_parameters; exit 0)
  ) option_parameters.helpkeys) in

  (* run-time check for key collisions *)
  let collision = Hashtbl.create (List.length actions) in
  List.iter (fun (key,_) ->
    if Hashtbl.mem collision key then
      eprintf "warning: option key %s has more than one meaning\n%!" key
    else
      Hashtbl.add collision key true
  ) actions;

  actions

let get_single_char_specs actions = 
  List.fold_left (fun acc (key,action) ->
    if String.length key = 2 && key.[0]='-' then (key.[1],action)::acc
    else acc) [] actions

(** print a usage message *)
let usage option_parameters = 
  let specs = getspecs option_parameters in
  let specs = List.sort (fun (a,_,_,_) (b,_,_,_) -> compare a b) specs in
  printf "Usage: %s " Sys.argv.(0);
  print_string option_parameters.usage_prefix;
  List.iter (fun (keys,_,arg,_) ->
    if keys <> ["--"] then
      printf "[%s%s] " (List.hd keys) (match arg with None -> "" | Some arg ->
        " " ^ arg)
  ) specs;
  printf "%s\n" option_parameters.usage_suffix

let rec _parse anonargs actions single_char_specs option_parameters options = function
  | first::rest when option_parameters.is_option first ->
      let first, rest = match option_parameters.parsetype with
      | Caml -> first, rest
      | Gnu -> 
          (* not sure how to mix -long options with Gnu... look them up first?*)
          try
          if String.length first > 2 && first.[0]='-' && first.[1]<>'-' then
            let firstchar = String.sub first 0 2 in
            let restchars = String.sub first 2 (String.length first - 2) in
              match List.assoc first.[1] single_char_specs with
              | Action _ -> firstchar,((sprintf "-%s" restchars)::rest)
              | _ -> firstchar,(restchars::rest)
          else first,rest
          with Not_found -> first,rest in
      let ap = 
        {options=options; option_parameters=option_parameters;args=rest} in
      (try match List.assoc first actions with
      | Action f -> _parse anonargs actions single_char_specs option_parameters
      (f ap) rest
      | Conversion f -> 
          (match rest with [] -> raise (MissingArgument first)
          | first :: rest -> _parse anonargs actions single_char_specs
          option_parameters (f ap first) rest)
      | Handler f -> f ap
       with Not_found -> raise (UnknownOption first))
  | [] -> options,(List.rev anonargs)
  | anonarg::rest as args -> 
      if option_parameters.keepgoing then
        _parse (anonarg::anonargs) actions single_char_specs option_parameters
        options rest
      else
        options,args

let parse option_parameters args = try
  let specs = getspecs option_parameters in
  let actions = getactions specs option_parameters in
  let single_char_specs = get_single_char_specs actions in
  _parse [] actions single_char_specs option_parameters option_parameters.default args
with UnknownOption s -> 
  eprintf "Unknown option %s\n" s; usage option_parameters; exit 0 
| ConversionFailure (v,t,o) -> 
  eprintf "Conversion failure; error converting %s to type %s for option %s\n" v
  t o; usage option_parameters; exit 0
| MissingArgument s -> 
  eprintf "Missing argument to option %s\n" s; usage option_parameters; exit 0

let parse_argv option_parameters = 
  parse option_parameters (List.tl (Array.to_list Sys.argv))

let set' f ap =
  parse {ap.option_parameters with default = f ap.options } ap.args

let conversion' f ap =
  match ap.args with [] -> raise (MissingArgument "")
  | next :: rest ->
  parse {ap.option_parameters with default = f ap.options next } rest

(** $Revision *)
