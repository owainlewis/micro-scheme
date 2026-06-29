type number =
  | Int of int
  | Float of float

type t =
  | Bool of bool
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Nil
  | Pair of t * t
  | Vector of t list
  | Bytevector of int list

let rec list = function
  | [] -> Nil
  | x :: xs -> Pair (x, list xs)

let dotted_list xs tail = List.fold_right (fun x acc -> Pair (x, acc)) xs tail

let rec to_proper_list = function
  | Nil -> Some []
  | Pair (a, d) -> Option.map (fun rest -> a :: rest) (to_proper_list d)
  | _ -> None

let rec to_list_prefix = function
  | Nil -> ([], None)
  | Pair (a, d) ->
      let xs, tail = to_list_prefix d in
      (a :: xs, tail)
  | other -> ([], Some other)

let escape_string s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let number_to_string = function
  | Int n -> string_of_int n
  | Float f ->
      let s = string_of_float f in
      if String.ends_with ~suffix:"." s then s ^ "0" else s

let char_to_string = function
  | ' ' -> "#\\space"
  | '\n' -> "#\\newline"
  | '\t' -> "#\\tab"
  | c -> "#\\" ^ String.make 1 c

let rec to_string = function
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Number n -> number_to_string n
  | Char c -> char_to_string c
  | String s -> "\"" ^ escape_string s ^ "\""
  | Symbol s -> s
  | Nil -> "()"
  | Pair _ as p -> "(" ^ pair_to_string p ^ ")"
  | Vector xs -> "#(" ^ String.concat " " (List.map to_string xs) ^ ")"
  | Bytevector xs ->
      "#u8(" ^ String.concat " " (List.map string_of_int xs) ^ ")"

and pair_to_string datum =
  let rec loop acc = function
    | Nil -> String.concat " " (List.rev acc)
    | Pair (a, d) -> loop (to_string a :: acc) d
    | tail ->
        String.concat " " (List.rev acc) ^ " . " ^ to_string tail
  in
  loop [] datum
