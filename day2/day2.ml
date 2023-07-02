(** day 2 **)

let input_filename = "input.txt"

(* part 1 *)
let () =
  let input = open_in input_filename in
  let surface_one_gift line =
    let l = String.split_on_char 'x' line
            |> List.map int_of_string 
            |> List.sort Int.compare
    in match l with
    | [x; y; z] -> x*y+ 2*x*y+2*x*z+2*y*z
    | _ -> failwith "data bugs"
  in
  let rec loop count =
    try 
      let line = input_line input in
      loop (count + (surface_one_gift line))
    with End_of_file -> count
  in
  loop 0 |> print_int; print_newline ();
  close_in input

(* part 2 *)
let () =
  let input = open_in input_filename in
  let ribbon_one_gift line =
    let l = String.split_on_char 'x' line
            |> List.map int_of_string 
            |> List.sort Int.compare
    in match l with
    | [x; y; z] -> 2*x+2*y+ x*y*z
    | _ -> failwith "data bugs"
  in
  let rec loop count =
    try 
      let line = input_line input in
      loop (count + (ribbon_one_gift line))
    with End_of_file -> count
  in
  loop 0 |> print_int; print_newline ();
  close_in input