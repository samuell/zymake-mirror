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
(** why aren't these part of the standard string library? *)

(** return true if character c is a whitespace character *)
let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

(** return a new string with leading and trailing whitespace removed *)
let strip s = 
  let i = ref 0 in
  while !i < String.length s && is_ws s.[!i] do incr i done;
  let j = ref (String.length s - 1) in
  while !j > !i && is_ws s.[!j] do decr j done;
  let len = !j - !i + 1 in
  if len <= 0 || !i >= String.length s then ""
  else String.sub s !i len

(** return a list of substrings of s, separated by whitespace *)
let split s = 
  let rec sub acc i = function
    None ->
      if i >= String.length s then List.rev acc 
      else if is_ws s.[i] then sub acc (i+1) None
      else sub acc (i+1) (Some i)
  | Some start ->
      if i >= String.length s || is_ws s.[i] then 
        sub ((String.sub s start (i-start))::acc) (i+1) None
      else sub acc (i+1) (Some start) in
  sub [] 1 (if is_ws s.[0] then None else Some 0)

