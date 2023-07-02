(** Day5 **)

let input_filename = "input.txt"


(* part 1 *)

let is_fine str =
  (* don't care about complexity *)
  let three_vowels =
    let vowels = ['a'; 'e'; 'i'; 'o'; 'u'] in
    let is_vowel c =
      if List.mem c vowels then 1 else 0
    in 
    String.fold_left (fun sum c -> is_vowel c +sum) 0 str >= 3
  in
  let n = String.length str in
  let rec has_double i =
    (* I'd need a String.existsi ... *)
    if i+1 >= n then false
    else str.[i] = str.[i+1] || has_double (i+1)
  in
  let no_forbidden_substring =
    let forbidden = ["ab"; "cd"; "pq"; "xy"] in
    let rec loop i =
      if i+1 >= n then true
      else not (List.exists (fun f -> String.sub str i 2 = f) forbidden) && loop (i+1)
    in loop 0
  in
  three_vowels && (has_double 0) && no_forbidden_substring

let () =
  let input = open_in input_filename in
  let rec loop count =
    try
      if is_fine (input_line input) then loop (count+1)
      else loop count
    with End_of_file -> count
  in
  loop 0 |> print_int; print_newline ();
  close_in input


(* Part 2 *)

(* I'll use the regexp Str module. 
   Kinda feel like I should have used that one in previous problems... oh well
*)

let is_really_nice str =
  (* still don't care about complexity *)
  let n = String.length str in
  let rec fst_condition i =
    if i+2 >= n then false
    else
      let factor =  String.sub str i 2 in
      Str.string_match (Str.regexp (".*"^factor^".*")) str (i+2)  || fst_condition (i+1)
  in
  let rec snd_condition i =
    if i+2 >= n then false
    else str.[i] = str.[i+2] || snd_condition (i+1)
  in
  fst_condition 0 && snd_condition 0

let () =
  let input = open_in input_filename in
  let rec loop count =
    try
      if is_really_nice (input_line input) then loop (count+1)
      else loop count
    with End_of_file -> count
  in
  loop 0 |> print_int; print_newline ();
  close_in input