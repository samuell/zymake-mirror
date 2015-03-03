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
open Printf
open Util

(* This module implements a simple scheme for running a process on the
 * least-loaded member of a set of remote machines.  It uses the /proc
 * filesystem to measure cpu load, and ssh to establish a connection.
 *)

type cache = {
  mutable last_update: float;
  mutable loads: (float * string) list;
  machines: string list;
  get_cpu_load: string;
}

let _getusage get_cpu_load (channel,machine) =
  (* get_cpu_load should be a program that writes to standard output the load on
  the machine *)
  let output = getoutput (sprintf "ssh -f -x %s %s" machine get_cpu_load) in
  let result = Scanf.sscanf output "%s %f" (fun _ result -> result) in
  Event.sync (Event.send channel (result,machine))

let getusage get_cpu_load machine =
  let ch = Event.new_channel() in
  let _ = Thread.create (_getusage get_cpu_load) (ch,machine) in
  (* wait 10 seconds; if you haven't gotten a response, return an 
   * infinite load *)
  (* FIXME: should I kill off the hung thread? *)
  Event.choose [Event.receive ch; watchdog 10. (infinity,machine)]

(* FIXME: need to catch breaks and exceptions during this process in
 * subthreads *)
let update_load_cache cache = 
  let events = List.map (getusage cache.get_cpu_load) cache.machines in
  cache.loads <- List.sort compare (List.map Event.sync events);
  if !verbose > 1 then begin
    List.iter (fun (load,machine) ->
      printf "%.2f %s\n" load machine
    ) cache.loads
  end;
  cache.last_update <- Sys.time ()

let new_cache machines get_cpu_load = 
  let c = { 
  last_update = 0.; loads = []; machines = machines; 
  get_cpu_load = get_cpu_load; } in
  update_load_cache c;
  c

let print_cache cache =
  printf "using program %s\n" cache.get_cpu_load;
  printf "last updated at time %s\n" (Util.strtime cache.last_update);
  List.iter (fun (load, machine) ->
    printf " %s %.2f\n" machine load
  ) cache.loads

let get_unloaded_machine cache =
  let now = Sys.time () in
  if cache.last_update < now -. 5. *. 60. (* last update was >=5 minutes ago *)
  || match cache.loads with [] -> true (* no unloaded machines in cache *)
     | (load,_) ::t -> load > 1. then (* least loaded machine has 1 full cpu *)
       update_load_cache cache;
  match cache.loads with [] -> failwith "no machines!"
  | (load,m)::rest -> 
      cache.loads <- rest;
      if load > 1. then 
        eprintf "warning: running on machine %s with load %f" m load;
      m
  let start_ssh cache dir = 
    let machine = get_unloaded_machine cache in
    if !verbose > 1 then 
      eprintf "creating node %s on machine %s\n%!" dir machine;
    sprintf "ssh -f -x %s \"cd %s; echo %s > %s/machine;\
      (sh %s/command; echo run complete) >%s/stdout 2>%s/stderr\"" 
      machine (Unix.getcwd ()) machine dir dir dir dir

let get_cpu_stat () =
  let f = open_in "/proc/stat" in
  let l = input_line f in
  close_in f;
  assert (l.[0] = 'c' && l.[1] = 'p' && l.[2] = 'u' && l.[3] = ' ');
  let p = ref 3 in
  while l.[!p]=' ' do incr p done;
  let start = !p in
  while l.[!p]<>' ' do incr p done;
  float_of_string (String.sub l start (!p-start))

let get_cpu_usage n =
  (* n-second snapshot estimate of CPU usage.  Uses /proc filesystem *)
  let a = Unix.time () in

  let user0 = get_cpu_stat () in
  Unix.sleep n;
  let c = Unix.time () in
  let user1 = get_cpu_stat () in
  (user1 -. user0) /. (c -. a) /. 100.0
