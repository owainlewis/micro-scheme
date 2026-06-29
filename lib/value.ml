open Error

type number = Datum.number =
  | Int of int
  | Float of float

type t =
  | Bool of bool
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Nil
  | Pair of pair
  | Vector of t array
  | Bytevector of int array
  | Primitive of primitive
  | Closure of closure
  | Promise of promise_state ref
  | Void

and pair = {
  car : t ref;
  cdr : t ref;
}

and primitive = {
  name : string;
  fn : t list -> t;
}

and params =
  | Fixed of string list
  | Variadic of string list * string

and closure = {
  params : params;
  body : Datum.t list;
  env : env;
}

and env = {
  bindings : (string, t ref) Hashtbl.t;
  parent : env option;
}

and promise_state =
  | Pending of (unit -> t)
  | Forced of t

let make_env ?parent () = { bindings = Hashtbl.create 64; parent }
let child_env parent = make_env ~parent ()

let rec lookup_cell env name =
  match Hashtbl.find_opt env.bindings name with
  | Some cell -> cell
  | None -> (
      match env.parent with
      | Some parent -> lookup_cell parent name
      | None -> errorf "unbound variable: %s" name)

let lookup env name = !(lookup_cell env name)
let define env name value = Hashtbl.replace env.bindings name (ref value)
let set env name value = lookup_cell env name := value

let extend parent names values =
  if List.length names <> List.length values then
    error "argument count mismatch";
  let env = child_env parent in
  List.iter2 (fun name value -> define env name value) names values;
  env

let pair a d = Pair { car = ref a; cdr = ref d }

let rec list = function
  | [] -> Nil
  | x :: xs -> pair x (list xs)

let rec to_list = function
  | Nil -> Some []
  | Pair p -> Option.map (fun rest -> !(p.car) :: rest) (to_list !(p.cdr))
  | _ -> None

let rec list_prefix = function
  | Nil -> ([], None)
  | Pair p ->
      let xs, tail = list_prefix !(p.cdr) in
      (!(p.car) :: xs, tail)
  | other -> ([], Some other)

let is_true = function Bool false -> false | _ -> true

let rec of_datum = function
  | Datum.Bool b -> Bool b
  | Datum.Number n -> Number n
  | Datum.Char c -> Char c
  | Datum.String s -> String s
  | Datum.Symbol s -> Symbol s
  | Datum.Nil -> Nil
  | Datum.Pair (a, d) -> pair (of_datum a) (of_datum d)
  | Datum.Vector xs -> Vector (Array.of_list (List.map of_datum xs))
  | Datum.Bytevector xs -> Bytevector (Array.of_list xs)

let rec to_datum = function
  | Bool b -> Datum.Bool b
  | Number n -> Datum.Number n
  | Char c -> Datum.Char c
  | String s -> Datum.String s
  | Symbol s -> Datum.Symbol s
  | Nil -> Datum.Nil
  | Pair p -> Datum.Pair (to_datum !(p.car), to_datum !(p.cdr))
  | Vector xs -> Datum.Vector (Array.to_list (Array.map to_datum xs))
  | Bytevector xs -> Datum.Bytevector (Array.to_list xs)
  | Primitive prim -> Datum.Symbol ("#<procedure:" ^ prim.name ^ ">")
  | Closure _ -> Datum.Symbol "#<procedure>"
  | Promise _ -> Datum.Symbol "#<promise>"
  | Void -> Datum.Symbol "#<void>"

let rec equal a b =
  match (a, b) with
  | Bool x, Bool y -> x = y
  | Number x, Number y -> x = y
  | Char x, Char y -> x = y
  | String x, String y -> String.equal x y
  | Symbol x, Symbol y -> String.equal x y
  | Nil, Nil -> true
  | Pair x, Pair y -> equal !(x.car) !(y.car) && equal !(x.cdr) !(y.cdr)
  | Vector x, Vector y ->
      Array.length x = Array.length y
      && Array.for_all2 equal x y
  | Bytevector x, Bytevector y -> x = y
  | Void, Void -> true
  | _ -> false

let eqv a b =
  match (a, b) with
  | Bool _, _ | Number _, _ | Char _, _ | Symbol _, _ | Nil, _ -> equal a b
  | Pair x, Pair y -> x == y
  | Vector x, Vector y -> x == y
  | Bytevector x, Bytevector y -> x == y
  | String x, String y -> x == y
  | Primitive x, Primitive y -> x.name = y.name
  | Closure x, Closure y -> x == y
  | Promise x, Promise y -> x == y
  | Void, Void -> true
  | _ -> false

let eq = eqv

let number_to_float = function Int n -> float_of_int n | Float f -> f

let force = function
  | Promise state -> (
      match !state with
      | Forced value -> value
      | Pending thunk ->
          let value = thunk () in
          state := Forced value;
          value)
  | other -> other

let write_string s = Datum.to_string (to_datum s)

let to_string = function
  | Primitive prim -> "#<procedure:" ^ prim.name ^ ">"
  | Closure _ -> "#<procedure>"
  | Promise _ -> "#<promise>"
  | Void -> "#<void>"
  | value -> Datum.to_string (to_datum value)

let display_string = function
  | String s -> s
  | Char c -> String.make 1 c
  | value -> to_string value
