open Error
open Value

let arity name expected got =
  if expected <> List.length got then
    errorf "%s expected %d arguments, got %d" name expected (List.length got)

let min_arity name expected got =
  if List.length got < expected then
    errorf "%s expected at least %d arguments, got %d" name expected
      (List.length got)

let expect_number name = function
  | Number n -> n
  | value -> errorf "%s expected number, got %s" name (to_string value)

let expect_int name = function
  | Number (Int n) -> n
  | value -> errorf "%s expected exact integer, got %s" name (to_string value)

let expect_pair name = function
  | Pair p -> p
  | value -> errorf "%s expected pair, got %s" name (to_string value)

let expect_string name = function
  | String s -> s
  | value -> errorf "%s expected string, got %s" name (to_string value)

let expect_symbol name = function
  | Symbol s -> s
  | value -> errorf "%s expected symbol, got %s" name (to_string value)

let expect_vector name = function
  | Vector v -> v
  | value -> errorf "%s expected vector, got %s" name (to_string value)

let expect_bytevector name = function
  | Bytevector v -> v
  | value -> errorf "%s expected bytevector, got %s" name (to_string value)

let bool b = Bool b

let number_add = function
  | [] -> Int 0
  | numbers
    when List.exists (function Float _ -> true | Int _ -> false) numbers ->
      Float (List.fold_left (fun acc n -> acc +. number_to_float n) 0.0 numbers)
  | numbers ->
      Int
        (List.fold_left
           (fun acc -> function Int n -> acc + n | Float _ -> assert false)
           0 numbers)

let number_mul = function
  | [] -> Int 1
  | numbers
    when List.exists (function Float _ -> true | Int _ -> false) numbers ->
      Float (List.fold_left (fun acc n -> acc *. number_to_float n) 1.0 numbers)
  | numbers ->
      Int
        (List.fold_left
           (fun acc -> function Int n -> acc * n | Float _ -> assert false)
           1 numbers)

let number_sub = function
  | [] -> error "- expected at least 1 argument"
  | [ Int n ] -> Int (-n)
  | [ Float f ] -> Float (-.f)
  | first :: rest
    when List.exists (function Float _ -> true | Int _ -> false) (first :: rest) ->
      Float
        (List.fold_left
           (fun acc n -> acc -. number_to_float n)
           (number_to_float first) rest)
  | Int first :: rest ->
      Int
        (List.fold_left
           (fun acc -> function Int n -> acc - n | Float _ -> assert false)
           first rest)
  | Float _ :: _ -> assert false

let number_div = function
  | [] -> error "/ expected at least 1 argument"
  | [ n ] -> Float (1.0 /. number_to_float n)
  | first :: rest ->
      Float
        (List.fold_left
           (fun acc n -> acc /. number_to_float n)
           (number_to_float first) rest)

let compare_chain name op args =
  min_arity name 2 args;
  let nums = List.map (expect_number name) args in
  let rec loop = function
    | [] | [ _ ] -> true
    | a :: (b :: _ as rest) -> op (number_to_float a) (number_to_float b) && loop rest
  in
  Bool (loop nums)

let primitive name fn = (name, Primitive { name; fn })

let copy_append args =
  let rec copy_list onto = function
    | [] -> onto
    | x :: xs -> pair x (copy_list onto xs)
  in
  match List.rev args with
  | [] -> Nil
  | last :: rest ->
      List.fold_left
        (fun acc value ->
          match to_list value with
          | Some xs -> copy_list acc xs
          | None -> error "append expected proper lists before final argument")
        last rest

let make_bindings () =
  [
    primitive "+" (fun args -> Number (number_add (List.map (expect_number "+") args)));
    primitive "-" (fun args -> Number (number_sub (List.map (expect_number "-") args)));
    primitive "*" (fun args -> Number (number_mul (List.map (expect_number "*") args)));
    primitive "/" (fun args -> Number (number_div (List.map (expect_number "/") args)));
    primitive "=" (compare_chain "=" ( = ));
    primitive "<" (compare_chain "<" ( < ));
    primitive ">" (compare_chain ">" ( > ));
    primitive "<=" (compare_chain "<=" ( <= ));
    primitive ">=" (compare_chain ">=" ( >= ));
    primitive "number?" (fun args ->
        arity "number?" 1 args;
        bool (match List.hd args with Number _ -> true | _ -> false));
    primitive "integer?" (fun args ->
        arity "integer?" 1 args;
        bool (match List.hd args with Number (Int _) -> true | _ -> false));
    primitive "zero?" (fun args ->
        arity "zero?" 1 args;
        bool (number_to_float (expect_number "zero?" (List.hd args)) = 0.0));
    primitive "not" (fun args ->
        arity "not" 1 args;
        bool (not (is_true (List.hd args))));
    primitive "boolean?" (fun args ->
        arity "boolean?" 1 args;
        bool (match List.hd args with Bool _ -> true | _ -> false));
    primitive "eq?" (fun args ->
        arity "eq?" 2 args;
        bool (eq (List.nth args 0) (List.nth args 1)));
    primitive "eqv?" (fun args ->
        arity "eqv?" 2 args;
        bool (eqv (List.nth args 0) (List.nth args 1)));
    primitive "equal?" (fun args ->
        arity "equal?" 2 args;
        bool (equal (List.nth args 0) (List.nth args 1)));
    primitive "cons" (fun args ->
        arity "cons" 2 args;
        pair (List.nth args 0) (List.nth args 1));
    primitive "car" (fun args ->
        arity "car" 1 args;
        let p = expect_pair "car" (List.hd args) in
        !(p.car));
    primitive "cdr" (fun args ->
        arity "cdr" 1 args;
        let p = expect_pair "cdr" (List.hd args) in
        !(p.cdr));
    primitive "set-car!" (fun args ->
        arity "set-car!" 2 args;
        (expect_pair "set-car!" (List.hd args)).car := List.nth args 1;
        Void);
    primitive "set-cdr!" (fun args ->
        arity "set-cdr!" 2 args;
        (expect_pair "set-cdr!" (List.hd args)).cdr := List.nth args 1;
        Void);
    primitive "pair?" (fun args ->
        arity "pair?" 1 args;
        bool (match List.hd args with Pair _ -> true | _ -> false));
    primitive "null?" (fun args ->
        arity "null?" 1 args;
        bool (match List.hd args with Nil -> true | _ -> false));
    primitive "list?" (fun args ->
        arity "list?" 1 args;
        bool (Option.is_some (to_list (List.hd args))));
    primitive "list" (fun args -> list args);
    primitive "length" (fun args ->
        arity "length" 1 args;
        match to_list (List.hd args) with
        | Some xs -> Number (Int (List.length xs))
        | None -> error "length expected a proper list");
    primitive "append" copy_append;
    primitive "reverse" (fun args ->
        arity "reverse" 1 args;
        match to_list (List.hd args) with
        | Some xs -> list (List.rev xs)
        | None -> error "reverse expected a proper list");
    primitive "symbol?" (fun args ->
        arity "symbol?" 1 args;
        bool (match List.hd args with Symbol _ -> true | _ -> false));
    primitive "symbol->string" (fun args ->
        arity "symbol->string" 1 args;
        String (expect_symbol "symbol->string" (List.hd args)));
    primitive "string->symbol" (fun args ->
        arity "string->symbol" 1 args;
        Symbol (expect_string "string->symbol" (List.hd args)));
    primitive "char?" (fun args ->
        arity "char?" 1 args;
        bool (match List.hd args with Char _ -> true | _ -> false));
    primitive "string?" (fun args ->
        arity "string?" 1 args;
        bool (match List.hd args with String _ -> true | _ -> false));
    primitive "string-length" (fun args ->
        arity "string-length" 1 args;
        Number (Int (String.length (expect_string "string-length" (List.hd args)))));
    primitive "string-append" (fun args ->
        String (String.concat "" (List.map (expect_string "string-append") args)));
    primitive "string-ref" (fun args ->
        arity "string-ref" 2 args;
        let s = expect_string "string-ref" (List.nth args 0) in
        let i = expect_int "string-ref" (List.nth args 1) in
        if i < 0 || i >= String.length s then error "string-ref index out of range";
        Char s.[i]);
    primitive "vector?" (fun args ->
        arity "vector?" 1 args;
        bool (match List.hd args with Vector _ -> true | _ -> false));
    primitive "vector" (fun args -> Vector (Array.of_list args));
    primitive "make-vector" (fun args ->
        min_arity "make-vector" 1 args;
        if List.length args > 2 then error "make-vector expected 1 or 2 arguments";
        let n = expect_int "make-vector" (List.hd args) in
        let fill = if List.length args = 2 then List.nth args 1 else Void in
        if n < 0 then error "make-vector expected non-negative length";
        Vector (Array.make n fill));
    primitive "vector-length" (fun args ->
        arity "vector-length" 1 args;
        Number (Int (Array.length (expect_vector "vector-length" (List.hd args)))));
    primitive "vector-ref" (fun args ->
        arity "vector-ref" 2 args;
        let v = expect_vector "vector-ref" (List.nth args 0) in
        let i = expect_int "vector-ref" (List.nth args 1) in
        if i < 0 || i >= Array.length v then error "vector-ref index out of range";
        v.(i));
    primitive "vector-set!" (fun args ->
        arity "vector-set!" 3 args;
        let v = expect_vector "vector-set!" (List.nth args 0) in
        let i = expect_int "vector-set!" (List.nth args 1) in
        if i < 0 || i >= Array.length v then error "vector-set! index out of range";
        v.(i) <- List.nth args 2;
        Void);
    primitive "vector->list" (fun args ->
        arity "vector->list" 1 args;
        list (Array.to_list (expect_vector "vector->list" (List.hd args))));
    primitive "list->vector" (fun args ->
        arity "list->vector" 1 args;
        match to_list (List.hd args) with
        | Some xs -> Vector (Array.of_list xs)
        | None -> error "list->vector expected proper list");
    primitive "bytevector?" (fun args ->
        arity "bytevector?" 1 args;
        bool (match List.hd args with Bytevector _ -> true | _ -> false));
    primitive "make-bytevector" (fun args ->
        min_arity "make-bytevector" 1 args;
        if List.length args > 2 then error "make-bytevector expected 1 or 2 arguments";
        let n = expect_int "make-bytevector" (List.hd args) in
        let fill = if List.length args = 2 then expect_int "make-bytevector" (List.nth args 1) else 0 in
        if n < 0 || fill < 0 || fill > 255 then error "invalid bytevector size or fill";
        Bytevector (Array.make n fill));
    primitive "bytevector-length" (fun args ->
        arity "bytevector-length" 1 args;
        Number
          (Int (Array.length (expect_bytevector "bytevector-length" (List.hd args)))));
    primitive "bytevector-u8-ref" (fun args ->
        arity "bytevector-u8-ref" 2 args;
        let v = expect_bytevector "bytevector-u8-ref" (List.nth args 0) in
        let i = expect_int "bytevector-u8-ref" (List.nth args 1) in
        if i < 0 || i >= Array.length v then error "bytevector-u8-ref index out of range";
        Number (Int v.(i)));
    primitive "bytevector-u8-set!" (fun args ->
        arity "bytevector-u8-set!" 3 args;
        let v = expect_bytevector "bytevector-u8-set!" (List.nth args 0) in
        let i = expect_int "bytevector-u8-set!" (List.nth args 1) in
        let n = expect_int "bytevector-u8-set!" (List.nth args 2) in
        if i < 0 || i >= Array.length v || n < 0 || n > 255 then
          error "bytevector-u8-set! index or value out of range";
        v.(i) <- n;
        Void);
    primitive "force" (fun args ->
        arity "force" 1 args;
        force (List.hd args));
    primitive "write" (fun args ->
        arity "write" 1 args;
        print_string (to_string (List.hd args));
        Void);
    primitive "display" (fun args ->
        arity "display" 1 args;
        print_string (display_string (List.hd args));
        Void);
    primitive "newline" (fun args ->
        arity "newline" 0 args;
        print_newline ();
        Void);
  ]

let install env =
  List.iter (fun (name, value) -> define env name value) (make_bindings ())
