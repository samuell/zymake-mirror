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
(* store a directed graph as an adjacency list; ins and outs are fully redundant
 *)
type 'a node ={
  mutable ins: 'a node list; 
  mutable outs: 'a node list; 
  data:'a; 
  i:int (* index of the node in the graph; not user-visible *)
}

type aux = int array

type 'a graph = 'a node array

let new_graph (outs: int -> int list) (elts: int -> 'a) n =
  let nodes = Array.init n (fun i -> {outs=[]; ins=[]; data=elts i;i=i}) in
  for i = 0 to n - 1 do
    nodes.(i).outs <- List.map (fun j -> nodes.(j)) (outs i);
    List.iter (fun y -> 
      y.ins <- nodes.(i) :: y.ins;
  ) nodes.(i).outs
  done;
  nodes

let reverse graph = 
  let n = Array.length graph in 
  new_graph (fun j -> List.map (fun n -> n.i) graph.(j).ins) (fun i -> graph.(i).data) n

let new_aux graph = Array.map (fun n -> List.length n.ins) graph

let get_roots graph num_ins =
  Array.fold_left (fun acc elt ->
    if num_ins.(elt.i)=0 then elt::acc else acc
  ) [] graph

let get_ready h num_ins =
  List.filter (fun x -> num_ins.(x.i) <- num_ins.(x.i) - 1; num_ins.(x.i) = 0) h.outs

let fold f acc graph =  
  let num_ins = new_aux graph in
  let roots = get_roots graph num_ins in
  let rec sub acc = function
  | [] -> acc
  | h::t -> sub (f acc h) (get_ready h num_ins @ t) in
  sub acc roots

let map f g =
  new_graph (fun i -> List.map (fun c -> c.i) g.(i).outs)
            (fun i -> f g.(i).data)
            (Array.length g)

let topo_map f g =
  let output = Array.make (Array.length g) None in
  fold (fun () cur -> 
    output.(cur.i) <- Some (
      let ins' = List.map (fun n ->
        match output.(n.i) with None -> invalid_arg "topo_map"
        | Some n -> n) cur.ins in
      f cur.data ins')
  ) () g;
  new_graph (fun i -> List.map (fun c -> c.i) g.(i).outs)
            (fun i -> match output.(i) with None -> invalid_arg "topo_map"
            | Some n -> n)
            (Array.length g)

let get_data x = x.data
let outs x = x.outs
let ins x = x.ins

open Printf

let dump g string_of_data =
  fold (fun () n -> 
    printf "[%d:" n.i;
    List.iter (fun o -> printf " %d" o.i) n.outs;
    printf "] %s\n" (string_of_data n.data)
  ) () g

let iter f g = Array.iter (fun n -> f n.data) g

let test () = begin
  let graph_of_array a = 
    new_graph (fun i -> snd a.(i)) (fun i -> fst a.(i)) (Array.length a) in
  let test_ok a expected =
    let g = graph_of_array a in
    let predicted = List.rev (fold (fun acc elt -> elt.data :: acc) [] g) in
    print_string (if predicted = expected then "Ok\n" else 
      sprintf "Failed; expected %s got %s\n" (String.concat " " expected)
      (String.concat " " predicted)) in
  let linear = [|("aa",[1]);("bb",[2]);("cc",[])|] in
  let branching = [|("aa",[1;2]);("bb",[3]);("cc",[3]);("dd",[])|] in
  (* FIXME; not a very complete set of tests *)
  test_ok linear ["aa";"bb";"cc"];
  test_ok branching ["aa";"bb";"cc";"dd"];
end
