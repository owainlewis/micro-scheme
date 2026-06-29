open Error

module D = Datum
module V = Value

let datum_list name datum =
  match D.to_proper_list datum with
  | Some xs -> xs
  | None -> errorf "%s expected a proper list" name

let parse_params = function
  | D.Symbol s -> V.Variadic ([], s)
  | D.Nil -> V.Fixed []
  | D.Pair _ as datum ->
      let rec loop names = function
        | D.Nil -> V.Fixed (List.rev names)
        | D.Symbol rest -> V.Variadic (List.rev names, rest)
        | D.Pair (D.Symbol name, tail) -> loop (name :: names) tail
        | other -> errorf "invalid lambda parameter list: %s" (D.to_string other)
      in
      loop [] datum
  | other -> errorf "invalid lambda parameter list: %s" (D.to_string other)

let bind_params parent params args =
  match params with
  | V.Fixed names -> V.extend parent names args
  | V.Variadic (names, rest) ->
      let fixed_count = List.length names in
      if List.length args < fixed_count then error "argument count mismatch";
      let rec split n left right =
        if n = 0 then (List.rev left, right)
        else
          match right with
          | [] -> error "argument count mismatch"
          | x :: xs -> split (n - 1) (x :: left) xs
      in
      let fixed, extra = split fixed_count [] args in
      let env = V.extend parent names fixed in
      V.define env rest (V.list extra);
      env

let rec eval env datum =
  match datum with
  | D.Bool _ | D.Number _ | D.Char _ | D.String _ | D.Vector _ | D.Bytevector _
    ->
      V.of_datum datum
  | D.Nil -> V.Nil
  | D.Symbol name -> V.lookup env name
  | D.Pair (D.Symbol "quote", rest) -> eval_quote rest
  | D.Pair (D.Symbol "if", rest) -> eval_if env rest
  | D.Pair (D.Symbol "lambda", rest) -> eval_lambda env rest
  | D.Pair (D.Symbol "define", rest) -> eval_define env rest
  | D.Pair (D.Symbol "set!", rest) -> eval_set env rest
  | D.Pair (D.Symbol "begin", rest) -> eval_begin env rest
  | D.Pair (D.Symbol "import", _) -> V.Void
  | D.Pair (D.Symbol "and", rest) -> eval_and env (datum_list "and" rest)
  | D.Pair (D.Symbol "or", rest) -> eval_or env (datum_list "or" rest)
  | D.Pair (D.Symbol "let", rest) -> eval_let env rest
  | D.Pair (D.Symbol "let*", rest) -> eval_let_star env rest
  | D.Pair (D.Symbol "letrec", rest) | D.Pair (D.Symbol "letrec*", rest) ->
      eval_letrec env rest
  | D.Pair (D.Symbol "cond", rest) -> eval_cond env (datum_list "cond" rest)
  | D.Pair (D.Symbol "case", rest) -> eval_case env rest
  | D.Pair (D.Symbol "when", rest) -> eval_when env rest
  | D.Pair (D.Symbol "unless", rest) -> eval_unless env rest
  | D.Pair (D.Symbol "delay", rest) -> eval_delay env rest
  | D.Pair (D.Symbol "delay-force", rest) -> eval_delay env rest
  | D.Pair (D.Symbol "quasiquote", rest) -> eval_quasiquote env rest
  | D.Pair (D.Symbol "do", rest) -> eval_do env rest
  | D.Pair (operator, operands) ->
      let proc = eval env operator in
      let args = List.map (eval env) (datum_list "procedure call" operands) in
      apply proc args

and eval_quote rest =
  match datum_list "quote" rest with
  | [ datum ] -> V.of_datum datum
  | _ -> error "quote expected 1 argument"

and eval_if env rest =
  match datum_list "if" rest with
  | [ test; consequent ] ->
      if V.is_true (eval env test) then eval env consequent else V.Void
  | [ test; consequent; alternate ] ->
      if V.is_true (eval env test) then eval env consequent else eval env alternate
  | _ -> error "if expected 2 or 3 arguments"

and eval_lambda env rest =
  match datum_list "lambda" rest with
  | params :: body when body <> [] ->
      V.Closure { params = parse_params params; body; env }
  | _ -> error "lambda expected parameters and body"

and eval_define env rest =
  match datum_list "define" rest with
  | [ D.Symbol name; expr ] ->
      let value = eval env expr in
      V.define env name value;
      V.Void
  | D.Pair (D.Symbol name, params) :: body when body <> [] ->
      let closure = V.Closure { params = parse_params params; body; env } in
      V.define env name closure;
      V.Void
  | _ -> error "invalid define form"

and eval_set env rest =
  match datum_list "set!" rest with
  | [ D.Symbol name; expr ] ->
      V.set env name (eval env expr);
      V.Void
  | _ -> error "set! expected variable and expression"

and eval_begin env rest = eval_sequence env (datum_list "begin" rest)

and eval_sequence env = function
  | [] -> V.Void
  | [ expr ] -> eval env expr
  | expr :: rest ->
      ignore (eval env expr);
      eval_sequence env rest

and eval_and env = function
  | [] -> V.Bool true
  | [ expr ] -> eval env expr
  | expr :: rest ->
      if V.is_true (eval env expr) then eval_and env rest else V.Bool false

and eval_or env = function
  | [] -> V.Bool false
  | [ expr ] -> eval env expr
  | expr :: rest ->
      let value = eval env expr in
      if V.is_true value then value else eval_or env rest

and binding_parts form =
  match datum_list "binding" form with
  | [ D.Symbol name; expr ] -> (name, expr)
  | _ -> errorf "invalid binding: %s" (D.to_string form)

and eval_let env rest =
  match datum_list "let" rest with
  | D.Symbol name :: bindings :: body when body <> [] ->
      let env' = V.child_env env in
      let binding_list = datum_list "let" bindings in
      let closure =
        V.Closure
          {
            params =
              V.Fixed
                (List.map
                   (fun binding ->
                     let name, _ = binding_parts binding in
                     name)
                   binding_list);
            body;
            env = env';
          }
      in
      V.define env' name closure;
      let args =
        List.map
          (fun binding ->
            let _, expr = binding_parts binding in
            eval env expr)
          binding_list
      in
      apply closure args
  | bindings :: body when body <> [] ->
      let pairs = List.map binding_parts (datum_list "let" bindings) in
      let names, exprs = List.split pairs in
      let values = List.map (eval env) exprs in
      eval_sequence (V.extend env names values) body
  | _ -> error "let expected bindings and body"

and eval_let_star env rest =
  match datum_list "let*" rest with
  | bindings :: body when body <> [] ->
      let env' = V.child_env env in
      List.iter
        (fun binding ->
          let name, expr = binding_parts binding in
          V.define env' name (eval env' expr))
        (datum_list "let*" bindings);
      eval_sequence env' body
  | _ -> error "let* expected bindings and body"

and eval_letrec env rest =
  match datum_list "letrec" rest with
  | bindings :: body when body <> [] ->
      let env' = V.child_env env in
      let pairs = List.map binding_parts (datum_list "letrec" bindings) in
      List.iter (fun (name, _) -> V.define env' name V.Void) pairs;
      List.iter (fun (name, expr) -> V.set env' name (eval env' expr)) pairs;
      eval_sequence env' body
  | _ -> error "letrec expected bindings and body"

and eval_cond env clauses =
  match clauses with
  | [] -> V.Void
  | clause :: rest -> (
      match datum_list "cond clause" clause with
      | [] -> error "empty cond clause"
      | D.Symbol "else" :: body -> eval_sequence env body
      | [ test ] ->
          let value = eval env test in
          if V.is_true value then value else eval_cond env rest
      | [ test; D.Symbol "=>"; receiver ] ->
          let value = eval env test in
          if V.is_true value then apply (eval env receiver) [ value ]
          else eval_cond env rest
      | test :: body ->
          if V.is_true (eval env test) then eval_sequence env body
          else eval_cond env rest)

and eval_case env rest =
  match datum_list "case" rest with
  | key :: clauses ->
      let key_value = eval env key in
      let rec loop = function
        | [] -> V.Void
        | clause :: more -> (
            match datum_list "case clause" clause with
            | D.Symbol "else" :: body -> eval_sequence env body
            | datums :: body ->
                let matches =
                  List.exists
                    (fun datum -> V.equal key_value (V.of_datum datum))
                    (datum_list "case datums" datums)
                in
                if matches then eval_sequence env body else loop more
            | [] -> error "empty case clause")
      in
      loop clauses
  | _ -> error "case expected key and clauses"

and eval_when env rest =
  match datum_list "when" rest with
  | test :: body ->
      if V.is_true (eval env test) then eval_sequence env body else V.Void
  | _ -> error "when expected test and body"

and eval_unless env rest =
  match datum_list "unless" rest with
  | test :: body ->
      if V.is_true (eval env test) then V.Void else eval_sequence env body
  | _ -> error "unless expected test and body"

and eval_delay env rest =
  match datum_list "delay" rest with
  | [ expr ] -> V.Promise (ref (V.Pending (fun () -> eval env expr)))
  | _ -> error "delay expected 1 expression"

and eval_quasiquote env rest =
  match datum_list "quasiquote" rest with
  | [ datum ] -> quasiquote env 1 datum
  | _ -> error "quasiquote expected 1 argument"

and quasiquote env depth datum =
  match datum with
  | D.Pair (D.Symbol "unquote", rest) when depth = 1 -> (
      match datum_list "unquote" rest with
      | [ expr ] -> eval env expr
      | _ -> error "unquote expected 1 expression")
  | D.Pair _ -> V.list (quasiquote_list env depth datum)
  | D.Vector xs -> V.Vector (Array.of_list (List.map (quasiquote env depth) xs))
  | _ -> V.of_datum datum

and quasiquote_list env depth datum =
  match datum with
  | D.Nil -> []
  | D.Pair (D.Pair (D.Symbol "unquote-splicing", rest), tail) when depth = 1 -> (
      match datum_list "unquote-splicing" rest with
      | [ expr ] -> (
          match V.to_list (eval env expr) with
          | Some xs -> xs @ quasiquote_list env depth tail
          | None -> error "unquote-splicing expected a proper list")
      | _ -> error "unquote-splicing expected 1 expression")
  | D.Pair (head, tail) -> quasiquote env depth head :: quasiquote_list env depth tail
  | tail -> [ V.Symbol "."; quasiquote env depth tail ]

and eval_do env rest =
  match datum_list "do" rest with
  | bindings :: test_clause :: body ->
      let specs =
        List.map
          (fun binding ->
            match datum_list "do binding" binding with
            | [ D.Symbol name; init ] -> (name, init, D.Symbol name)
            | [ D.Symbol name; init; step ] -> (name, init, step)
            | _ -> error "invalid do binding")
          (datum_list "do bindings" bindings)
      in
      let test_items = datum_list "do test" test_clause in
      let test, result =
        match test_items with
        | [] -> error "do test clause cannot be empty"
        | test :: result -> (test, result)
      in
      let rec loop values =
        let env' = V.child_env env in
        List.iter2 (fun (name, _, _) value -> V.define env' name value) specs values;
        if V.is_true (eval env' test) then eval_sequence env' result
        else (
          ignore (eval_sequence env' body);
          let next_values = List.map (fun (_, _, step) -> eval env' step) specs in
          loop next_values)
      in
      loop (List.map (fun (_, init, _) -> eval env init) specs)
  | _ -> error "do expected bindings, test, and body"

and apply proc args =
  match proc with
  | V.Primitive prim -> prim.fn args
  | V.Closure closure ->
      let env = bind_params closure.env closure.params args in
      eval_sequence env closure.body
  | value -> errorf "attempted to call non-procedure: %s" (V.to_string value)

let initial_env () =
  let env = V.make_env () in
  Primitives.install env;
  env

let eval_all env datums = eval_sequence env (List.map Expand.expand datums)
let eval_string env source = eval_all env (Reader.read_all source)
let eval_file env path = eval_string env (In_channel.with_open_text path In_channel.input_all)
