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
open Printf
open Parseopt
open Util

(* glossary *)
(* node: a file, specified by a set of key-value pairs plus an optional suffix.
 * rule: a command which will be executed by the shell, parameterized by
 *  a set of input nodes, output nodes, and expressions
 * input node: a node required by a given rule
 * output node: a node created by a given rule
 * "pushdown": the process of removing keys from a node that aren't used
   either by it or any node it depends on.  This allows common dependents to
   be shared.
 *)

type opts =
  { runmode :
    [ `Normal
    | `Printload
    | `Printdag
    | `Query of string
    | `Start of string
    | `Listloads
    | `Update ];
    system : [ `System | `Dryrun | `SshRemoteStart | `UserStart of string ];
    long : int;
    interactive : bool;
    max_label_len : int;
    print_revisions : bool;
    print_version : bool;
    message : [ `Command | `Description | `Both | `Neither ];
    machines : string list;
    includekeys : bool;
    verbose : int }
let opts =
  {(Parseopt.option_defaults) with
    Parseopt.default =
      {runmode = `Normal; system = `System; long = 60; interactive = true;
       max_label_len = 8; print_revisions = false; print_version = false;
       message = `Command; machines = []; includekeys = false; verbose = 0};
    Parseopt.specs =
      ["verbose", [], "increase verbosity output", None,
       Parseopt.Action
         (fun ap ->
            {(ap.Parseopt.options) with
              verbose = (fun a -> a.options.verbose + 1) ap});
       "includekeys", ["-k"; "--includekeys"], "include keys in filenames",
       None,
       Parseopt.Action
         (fun ap -> {(ap.Parseopt.options) with includekeys = set ap});
       "machines", ["-l"; "--parallel"],
       "run in parallel on MACHINES (space-separated list)", Some "MACHINES",
       Parseopt.Conversion
         (fun ap s ->
            {(ap.Parseopt.options) with
              machines = (fun ap s -> Mystring.split s) ap s});
       "message", [], "what to print for each command (command | description | both | neither) default is command",
       Some "MESSAGE",
       Parseopt.Conversion
         (fun ap s ->
            {(ap.Parseopt.options) with
              message =
                match s with
                  "neither" -> `Neither
                | "both" -> `Both
                | "description" -> `Description
                | "command" -> `Command
                | _ ->
                    raise
                      (Parseopt.ConversionFailure
                         (s, "command|description|both|neither",
                          "message"))});
       "print_version", ["-V"], "print zymake version", None,
       Parseopt.Action
         (fun ap -> {(ap.Parseopt.options) with print_version = set ap});
       "print_revisions", ["-R"],
       "print the revision status of all source files", None,
       Parseopt.Action
         (fun ap -> {(ap.Parseopt.options) with print_revisions = set ap});
       "max_label_len", ["-x"; "--max-label-len"],
       "maximum length of a label assigned to a key, value pair",
       Some "MAX_LABEL_LEN",
       Parseopt.Conversion
         (fun ap s ->
            {(ap.Parseopt.options) with
              max_label_len =
                try int_of_string s with
                  Failure "int_of_string" ->
                    raise
                      (Parseopt.ConversionFailure
                         (s, "int", "max_label_len"))});
       "interactive", [],
       "require confirmation before overwriting long-computation outputs",
       None,
       Parseopt.Action
         (fun ap -> {(ap.Parseopt.options) with interactive = set ap});
       "UNK", ["-f"; "--force"],
       "proceed with build even if it overwrites long-computation outputs",
       None, Handler (set' (fun o -> {o with interactive = false}));
       "long", ["--long"],
       "how many seconds counts as a long-computation job (default 1 min)",
       Some "LONG",
       Parseopt.Conversion
         (fun ap s ->
            {(ap.Parseopt.options) with
              long =
                try int_of_string s with
                  Failure "int_of_string" ->
                    raise (Parseopt.ConversionFailure (s, "int", "long"))});
       "system", ["-d"; "--dontmake"], "don't actually run commands", None,
       Handler (set' (fun o -> {o with system = `Dryrun}));
       "UNK", ["-r"; "--remotestart"], "", None,
       Handler (set' (fun o -> {o with system = `SshRemoteStart}));
       "UNK", ["-c"; "--command"],
       "command to start remote programs (default: internal)", None,
       Handler (conversion' (fun o s -> {o with system = `UserStart s}));
       "runmode", ["-p"; "--printdag"], "print in dag format for topodiff",
       None, Handler (set' (fun o -> {o with runmode = `Printdag}));
       "UNK", ["-a"; "--printload"], "print load on this machine", None,
       Handler (set' (fun o -> {o with runmode = `Printload}));
       "UNK", ["-q"; "--query"],
       "print to stdout filename associated with QUERY", None,
       Handler (conversion' (fun o s -> {o with runmode = `Query s}));
       "UNK", ["-s"; "--start"], "start program START on MACHINES", None,
       Handler (conversion' (fun o s -> {o with runmode = `Start s}));
       "UNK", ["-t"; "--listloads"], "list loads on machines", None,
       Handler (set' (fun o -> {o with runmode = `Listloads}));
       "UNK", ["--update-times"], "touch all existing output files in build order.  fix for bad timestamps due to network problems",
       None, Handler (set' (fun o -> {o with runmode = `Update}))];
    Parseopt.to_string =
      fun record ->
        Printf.sprintf "verbose = %s
includekeys = %s
machines = %s
message = %s
print_version = %s
print_revisions = %s
max_label_len = %s
interactive = %s
long = %s
system = %s
runmode = %s
"
          (let field = record.verbose in string_of_int field)
          (let field = record.includekeys in string_of_bool field)
          (let field = record.machines in
           ignore field; "(user-specified data)")
          (let field = record.message in
           match field with
             `Command -> "command"
           | `Description -> "description"
           | `Both -> "both"
           | `Neither -> "neither")
          (let field = record.print_version in string_of_bool field)
          (let field = record.print_revisions in string_of_bool field)
          (let field = record.max_label_len in string_of_int field)
          (let field = record.interactive in string_of_bool field)
          (let field = record.long in string_of_int field)
          (let field = record.system in ignore field; "(user-specified data)")
          (let field = record.runmode in
           ignore field; "(user-specified data)")}

let opts =
  {opts with usage_suffix = "zymakefile"; keyspecs = [Short; Long2];
    parsetype = Gnu}

let () =
  Sys.catch_break true;
  let (options, args) = parse_argv opts in
  if options.print_version then
    begin
      printf "zymake; version %s %s\n" Version.version
        (if Version.up_to_date then ""
         else "(modified; use -R for more detail)");
      exit 0
    end;
  if options.print_revisions then
    begin printf "zymake; revisions %s\n" Version.revision; exit 0 end;
  if options.verbose > 0 then
    printf "options chosen: %s\n" (opts.to_string options);
  verbose := options.verbose;
  match options.runmode with
    `Start program ->
      ignore
        (Unix.system
           (Load.start_ssh (Load.new_cache options.machines "zymake -a")
              program))
  | `Listloads ->
      let cache = Load.new_cache options.machines "zymake -a" in
      Load.print_cache cache
  | `Printload ->
      printf "%s %.2f\n" (Unix.gethostname ()) (Load.get_cpu_usage 4)
  | _ ->
      let makefilefn =
        match args with
          [arg] -> arg
        | _ -> usage opts; exit 1
      in
      if !verbose > 0 then eprintf "Parsing makefile...\n%!";
      let groups =
        Lexer.parse_file makefilefn Parser.groups Lexer.start_of_line
      in
      if !verbose > 0 then eprintf "Parsing dictionary file...\n%!";
      let dictfn = Firule.make_dictfn makefilefn in
      let dictionary =
        if Sys.file_exists dictfn then
          Lexer.parse_file dictfn Parser.dictionary
            Lexer.start_dictionary_line
        else []
      in
      let query =
        match options.runmode with
          `Query query ->
            let lexbuf = Lexing.from_string (query ^ "\n") in
            Lexer.parse "query" lexbuf Parser.sentence Lexer.content_of_line
        | _ -> []
      in
      if !verbose > 0 then eprintf "Compiling makefile...\n%!";
      let dorun = options.runmode <> `Printdag && options.system <> `Dryrun in
      let (keys, empty_locals, globals, orules, roots, dictm, query) =
        Compile.compile_makefile groups dictionary query
      in
      if options.verbose > 0 then printf "Construct dependency tree...\n%!";
      let irules =
        Dag.construct_dependency_dag keys empty_locals globals orules roots
      in
      if options.verbose > 0 then printf "Interpolate strings...\n%!";
      let dict =
        Firule.make_dict keys makefilefn options.includekeys empty_locals
          dorun options.max_label_len dictm globals dictfn
      in
      let firules = Topo.topo_map (Firule.firule_of_irule dict) irules in
      Firule.check_long_computations firules options.long options.interactive;
      match options.runmode with
        `Printdag -> Topo.dump (Topo.reverse firules) (fun d -> d.fi_command)
      | `Query _ -> printf "%s\n" (Firule.string_of_command dict query)
      | `Update -> Topo.fold Build.touch_node () firules
      | _ ->
          Build.build firules options.message options.system options.machines
