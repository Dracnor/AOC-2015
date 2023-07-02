(** day 4 **)

let input = "ckczppom"


(* part 1 *)

let five_leading_zeros str =
  List.init 5 (fun i -> i) 
  |> List.for_all (fun i -> str.[i] = '0')

let rec loop str i =
  let hash = str ^ (string_of_int i) |> Digest.string |> Digest.to_hex in
  if five_leading_zeros hash then i
  else loop str (i + 1)

let () =
  loop input 1 |> print_int; print_newline ()
  

(* part 2 *)
let six_leading_zeros str =
  List.init 6 (fun i -> i) 
  |> List.for_all (fun i -> str.[i] = '0')

let rec loop str i =
  let hash = str ^ (string_of_int i) |> Digest.string |> Digest.to_hex in
  if six_leading_zeros hash then i
  else loop str (i + 1)

let () =
  loop input 1 |> print_int; print_newline ()