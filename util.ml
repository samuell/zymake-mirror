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
(* package-wide verbosity *)
let verbose = ref 0

(* some generic functions of general utility *)

(**list_join sep [l1;l2;...] is [l1;sep;l2;sep;...]; lists of length <2 no chg*)
let list_join sep = function
  ([] | [_]) as l -> l
| h::t ->
    let rec sub acc = function
      [] -> List.rev acc
    | h::t -> sub (h::sep::acc) t in
    sub [h] t

(** return true if fn is the name of a file that exists *)
let file_exists fn = try Unix.access fn [Unix.F_OK]; true
  with Unix.Unix_error _ -> false

(** return the last modification date of fn, or neg_infinity if it's not there*)
let get_time fn = try `Timestamp ((Unix.stat fn).Unix.st_mtime, fn)
  with Unix.Unix_error(Unix.ENOENT,_,_) -> `OutputMissing fn

let days = [|"Sun";"Mon";"Tue";"Wed";"Thu";"Fri";"Sat"|]
let months =
  [|"Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"|]
let strtime t = 
  let tm = Unix.localtime t in
  Printf.sprintf "%s %s %2d %2d:%02d:%02d TZ %04d"
    days.(tm.Unix.tm_wday) months.(tm.Unix.tm_mon) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec (tm.Unix.tm_year+1900)

(** return the same list, but with only one physically equal copy of each 
  * element.  O(n^2). *)
let uniq l =
  let rec sub acc = function
  | [] -> acc
  | h::t -> sub (if List.memq h acc then acc else h::acc) t in
  sub [] l

(** return the same list, but with only one structurally equal copy of each 
  * element.  O(n^2). *)
let uniq_struct l =
  let rec sub acc = function
  | [] -> acc
  | h::t -> sub (if List.mem h acc then acc else h::acc) t in
  sub [] l

(** like Perl/shell backticks; return the standard output of a command, minus
    the final newline. *)
let getoutput command = 
  let ret = Buffer.create 10 in
  let pipe = Unix.open_process_in command in
  let first_line = ref true in
  (try while true do
    let line = input_line pipe in 
    if !first_line then first_line := false
    else Buffer.add_char ret '\n';
    Buffer.add_string ret line;
  done with End_of_file -> ignore (Unix.close_process_in pipe)); 
  (* FIXME deal with the return value of close_process_in*)
  Buffer.contents ret

(* watchdog code from http://alan.petitepomme.net/cwn/2005.01.25.html#3
 * (Caml Weekly News) *)
(** watchdog s v returns the event of receiving value v in s seconds *)
let watchdog s v =
  let ch = Event.new_channel () in
  let watchdog_thread () =
    Thread.delay s;
    Event.sync (Event.send ch v) in
  ignore (Thread.create watchdog_thread ());
  Event.receive ch

let is_whitespace s = 
  let l = String.length s in 
  let rec sub i =
    if i=l then true
    else
      let c = s.[i] in
      (c = ' ' || c = '\t' || c = '\n' || c = '\r') && sub (i+1) in
  sub 0

let repr ?(size=10) f x = let b = Buffer.create size in f b x; Buffer.contents b

(** range n = [0;1;...;n-1] *)
let range n =   let rec sub acc n = if n < 0 then acc else sub (n::acc) (n-1) in
sub [] (n-1)

(* rm -rf - VERY DANGEROUS *)
let rec deltree path = 
  if (Unix.stat path).Unix.st_kind = Unix.S_DIR then begin
    Array.iter (fun fn ->
      deltree (Filename.concat path fn)
    ) (Sys.readdir path);
    Unix.rmdir path
  end else
    Sys.remove path

(** explode_string "abcd" = ['a';'b';'c';'d'] *)
let explode_string s = 
  let rec sub acc i = 
    if i<0 then acc
    else sub (s.[i]::acc) (i-1) in
  sub [] (String.length s - 1)

(** implode_string ['a';'b';'c';'d'] = "abcd" *)
let implode_string l = 
  let len = List.length l in
  let s = String.create len in
  let rec sub i = function
    | [] -> ()
    | c :: tl -> s.[i] <- c; sub (i+1) tl in
  sub 0 l;
  s

