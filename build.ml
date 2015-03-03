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
(** this file contains the code used to actually build a dag of nodes *)
(* that is, to execute the commands on one or more machines in 
 * topological order, and handle build errors *)

open Types
open Util
open Printf

(** code for parallel execution *)

type cmd_error = Returned of Unix.process_status | Exception of exn
exception FailedBuild of cmd_error * firule list
type return = Succeeded | Failed of cmd_error

let write_string_to_file s filename =
  let outf = open_out filename in
  fprintf outf "%s" s;
  close_out outf

let catch_system wrap_command irule = 
  let start = Unix.time () in
  let ret = try
    let command = wrap_command irule.fi_command irule.dir in
    (match Unix.system command with
    | Unix.WEXITED 0 -> Succeeded 
    | r -> Failed (Returned r))
  with e ->
    Failed(Exception e) in
  let timef = open_out (irule.dir ^ "/time") in
  fprintf timef "%g\n" (Unix.time () -. start);
  close_out timef;
  ret

type run_context = {
  aux: Topo.aux;
  message: [`Description | `Both | `Command | `Neither];
  start: firule -> return;
}

let run (rc,cell,channel) = 
  let return = rc.start (Topo.get_data cell) in
  Event.sync (Event.send channel (cell,return))

let listen rc node =
  let ch = Event.new_channel() in
  let _ = Thread.create run (rc,node,ch) in
  node,Event.receive ch

let should_build rc node = 
  let irule = Topo.get_data node in
  match irule.last_modification with `InputRebuilt _ ->
    (match rc.message with 
      `Description | `Both -> printf "%s\n%!" irule.irule.i_rule.comment
      | _ -> ());
    (match rc.message with 
      `Command | `Both -> printf "%s\n%!" irule.fi_command
      | _ -> ());
    true
  | _ -> false

let rec wait rc = function
| [] -> () (* done! *) 
| jobs -> 
    let finished = match Event.select (List.map snd jobs) with
    | finished, Succeeded -> finished
    | _,Failed r -> raise (FailedBuild(r,List.map (fun (i,_) ->
        (Topo.get_data i)) jobs)) in
    let finishedrule = (Topo.get_data finished) in
    if !verbose > 1 then eprintf "command %s finished\n%!" finishedrule.descr;
    let jobs = List.filter (fun (n,_) -> n != finished) jobs in
    parallel_build rc jobs (Topo.get_ready finished rc.aux)

(* parallel_build rc jobs_running jobs_whose_prereqs_are_done *)
and parallel_build rc jobs = function
| [] -> wait rc jobs
| ready_node::rest ->
    let dobuild = 
      try should_build rc ready_node
      with e -> raise (FailedBuild(Exception e,List.map (fun (i,_) ->
        (Topo.get_data i)) jobs)) in
    if dobuild then
      parallel_build rc ((listen rc ready_node)::jobs) rest
    else
      parallel_build rc jobs ((Topo.get_ready ready_node rc.aux)@rest)

let serial_build rc () node = 
  let irule = Topo.get_data node in
  match
    (try
       if should_build rc node then rc.start irule
       else Succeeded
     with e -> raise (FailedBuild(Exception e,[irule])))
  with Succeeded -> ()
  | Failed r -> raise (FailedBuild(r,[irule]))

(* this should be the only public function *)

(** if machines is an empty list, the request is to build the dag on the
* current machine; otherwise, the machines listed will be used *)
let build outs message system machines =
    try
      let start = match system with
      (* note: this requires that this program be callable by this name on
         the target machine (i.e. it's in your path). *)
      | `SshRemoteStart -> catch_system (fun s dir -> 
          write_string_to_file s (dir ^ "/command");
          (Load.start_ssh (Load.new_cache machines "zymake -a") dir))
      | `System -> catch_system (fun s _ -> s)
      | `UserStart cmd -> catch_system (fun s dir -> 
          write_string_to_file s (dir ^ "/command");
          (cmd ^ " " ^ dir ^ "/command"))
      | `Dryrun -> fun _ -> Succeeded in
      let rc = {
        aux = Topo.new_aux outs;
        message = message;
        start = start;
      } in
      match machines with 
      | [] -> 
        Topo.fold (serial_build rc) () outs

      | machines ->
        let roots = Topo.get_roots outs rc.aux in
        parallel_build rc [] roots
    with 
    (* catch Sys.Break too... FIXME *)
      FailedBuild(problem,irules) -> begin
        eprintf "build error\n";
        List.iter (fun irule ->
          (* move aside, don't delete...? *)
          let outputs = irule.dir::irule.outputfns in
          List.iter deltree outputs;
          eprintf "command '%s' killed; deleted output(s) %s\n"
            irule.fi_command (String.concat " " outputs)
        ) irules;
        flush stderr;
        (match problem with
          Exception(Sys.Break) -> eprintf "User Break\n%!"
        | Returned (Unix.WEXITED i) ->
            eprintf "process executed normally but with status %d\n" i
        | Returned (Unix.WSIGNALED i) ->
            eprintf "process killed by signal %d\n" i
        | Returned (Unix.WSTOPPED i) ->
            eprintf "process stopped by signal %d\n" i
        | Exception e -> raise e
        )
      end

(** Effect: update last modification time of `filename' to current time.
  * No effect if file does not exist or if cannot be modified. *)
let touch filename =
  try
    Unix.utimes filename (Unix.stat filename).Unix.st_atime (Unix.time ())
  with _ -> ()

let touch_node () node = List.iter touch (Topo.get_data node).outputfns

