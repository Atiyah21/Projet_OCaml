type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let rec explode (str : string) : char list =
        if (String.length str) > 0 then [str.[0]]@explode (String.sub str 1 (String.length str - 1))
        else []
;; 


(* conversions *)
let base_of_char (c : char) : base =
  match c with
  | 'A' -> A
  | 'T' -> T
  | 'G' -> G
  | 'C' -> C
  | _ -> WC
;;

let dna_of_string (s : string) : base list =
  List.map base_of_char (explode s)
;;


let rec string_of_dna (seq : dna) : string =
  match seq with
  | [] -> ""
  | h::t -> string_of_base h ^ string_of_dna t



(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  let rec cut_search pre suf =
    match pre,suf with
    | [], suf -> Some(suf)
    | h1::t1, h2::t2 when h1=h2 -> cut_search t1 t2
    | _ -> None 
  in cut_search slice list

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)

let rec sub l a b =
        if a = 0 && b = 0 then []
        else
  match l with
  | [] -> []
  | hd :: tl ->
    if a > 0 then
      sub tl (a-1) (b-1)
    else if b >= 0 then
      hd :: sub tl 0 (b-1)
    else
      []

let first_occ (slice : 'a list) (list : 'a list) : ('a list * 'a list) option =
        let rec aux l s i =
                if sub l i (i + List.length s) = s then Some (sub l 0 i, sub l (i + List.length s) (List.length l))
                else if i + List.length s < List.length l then aux l s (i + 1)
                else None
        in aux list slice 0
;;
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  let rec slices_search stock l =
    let occ_start = first_occ start l in
    match occ_start with
    |None -> stock
    |Some(l1,l2) ->
      let occ_stop = first_occ stop l2 in
      match occ_stop with
      |None -> stock
      |Some(l3,l4) ->
        match l4 with
        |[] -> stock
        |_ -> slices_search (l3::stock) l4
  in
  List.rev (slices_search [] list)
        

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]]
 *)

let cut_genes (dna : dna) : (dna list) =
  failwith "A faire"

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)
let consensus (list : 'a list) : 'a consensus =
  failwith "À compléter"

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

let consensus_sequence (ll : 'a list list) : 'a consensus list =
  failwith "À compléter"

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
