(** Day1 **)

let input_file = "input.txt"

(* part1 *)
let () =
  let input = open_in input_file in
  let str = input_line input in
  let n = String.length str in
  let rec loop floor i =
    if i >=n then floor
    else if str.[i] = '(' then loop (floor+1) (i+1)
    else if str.[i] = ')' then loop (floor-1) (i+1)
    else failwith "bug"
  in
  loop 0 0 |> print_int; print_newline ();
  close_in input

(* part2 *)
let () =
  let input = open_in input_file in
  let str = input_line input in
  let n = String.length str in
  let rec loop floor i =
    if floor == -1 then i
    else if i >= n then failwith "-1 never reached"
    else if str.[i] = '(' then loop (floor+1) (i+1)
    else if str.[i] = ')' then loop (floor-1) (i+1)
    else failwith "bug"
  in
  loop 0 0 |> print_int; print_newline ();
  close_in input
