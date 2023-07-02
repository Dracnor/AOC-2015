(** Day3 **)

module IntPairs = (* pasted from v2.ocaml.org *)
       struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Stdlib.compare x0 x1 with
               0 -> Stdlib.compare y0 y1
             | c -> c
       end
module PairsSet = Set.Make(IntPairs)


let input_filename = "input.txt"

(* part 1 *)
let () =
  let input = open_in input_filename in
  let str = input_line input in
  let n = String.length str in
  let rec loop i x y set = 
    if i >= n then set
    else
      let (new_x, new_y) = match str.[i] with
        | '^' -> (x, y+1) 
        | 'v' -> (x, y-1)
        | '<' -> (x-1, y)
        | '>' -> (x+1, y)
        | _ -> failwith "parsing error"
      in
      PairsSet.add (new_x, new_y) set |> loop (i+1) new_x new_y
  in
  (* /!\ origin is reached from start *)
  loop 0 0 0 (PairsSet.singleton (0,0))
  |> PairsSet.cardinal
  |> print_int; print_newline ();
  close_in input


(* part2. 
   Same but two paths and union them at the end.
   Numbers of char is even.
*)

let new_coo (x,y) = function
  | '^' -> (x, y+1)
  | 'v' -> (x, y-1)
  | '<' -> (x-1, y)
  | '>' -> (x+1, y)
  | _ -> failwith "parsing error"

let () =
  let input = open_in input_filename in
  let str = input_line input in
  let n = String.length str in
  let rec loop i coo_santa santa coo_robo robo =
    (** i : index in the string; 
        coo_X : current position
        of caracter X;
        X : set of past positions of cacacter. X 
    *) 
    if i+1 >= n then santa, robo
    else 
      let new_coo_santa = new_coo coo_santa str.[i] in
      let new_santa = PairsSet.add new_coo_santa santa in
      let new_coo_robo = new_coo coo_robo str.[i+1] in
      let new_robo = PairsSet.add new_coo_robo robo in
      loop (i+2) new_coo_santa new_santa new_coo_robo new_robo
  in
  (* /!\ origin is reached from start *)
  let origin = (0,0) in
  let santa, robo = loop 0 
                         origin (PairsSet.singleton origin) 
                         origin (PairsSet.singleton origin) in 
  PairsSet.union santa robo
  |> PairsSet.cardinal
  |> print_int; print_newline ();
  close_in input