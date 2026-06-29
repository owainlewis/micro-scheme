open Error
open Datum

type parser = {
  source : string;
  mutable pos : int;
  len : int;
}

let of_string source = { source; pos = 0; len = String.length source }
let eof p = p.pos >= p.len
let peek p = if eof p then None else Some p.source.[p.pos]

let next p =
  match peek p with
  | None -> None
  | Some c ->
      p.pos <- p.pos + 1;
      Some c

let expect p c =
  match next p with
  | Some got when got = c -> ()
  | Some got -> errorf "expected '%c', got '%c'" c got
  | None -> errorf "expected '%c', got end of input" c

let starts_with p text =
  let n = String.length text in
  p.pos + n <= p.len && String.sub p.source p.pos n = text

let is_delimiter = function
  | ' ' | '\n' | '\r' | '\t' | '(' | ')' | '"' | ';' -> true
  | _ -> false

let rec skip p =
  if not (eof p) then
    match peek p with
    | Some (' ' | '\n' | '\r' | '\t') ->
        ignore (next p);
        skip p
    | Some ';' ->
        skip_line p;
        skip p
    | Some '#' when starts_with p "#|" ->
        p.pos <- p.pos + 2;
        skip_block_comment p 1;
        skip p
    | Some '#' when starts_with p "#;" ->
        p.pos <- p.pos + 2;
        ignore (parse_datum p);
        skip p
    | _ -> ()

and skip_line p =
  match next p with
  | None | Some '\n' -> ()
  | _ -> skip_line p

and skip_block_comment p depth =
  if depth = 0 then ()
  else if eof p then error "unterminated block comment"
  else if starts_with p "#|" then (
    p.pos <- p.pos + 2;
    skip_block_comment p (depth + 1))
  else if starts_with p "|#" then (
    p.pos <- p.pos + 2;
    skip_block_comment p (depth - 1))
  else (
    ignore (next p);
    skip_block_comment p depth)

and parse_datum p =
  skip p;
  match next p with
  | None -> error "unexpected end of input"
  | Some '(' -> parse_list p
  | Some '\'' -> list [ Symbol "quote"; parse_datum p ]
  | Some '`' -> list [ Symbol "quasiquote"; parse_datum p ]
  | Some ',' ->
      if starts_with p "@" then (
        p.pos <- p.pos + 1;
        list [ Symbol "unquote-splicing"; parse_datum p ])
      else list [ Symbol "unquote"; parse_datum p ]
  | Some '"' -> parse_string p
  | Some '|' -> Symbol (parse_bar_symbol p)
  | Some '#' -> parse_hash p
  | Some c -> parse_atom p c

and parse_list p =
  skip p;
  match peek p with
  | Some ')' ->
      ignore (next p);
      Nil
  | _ ->
      let rec elements acc =
        skip p;
        match peek p with
        | None -> error "unterminated list"
        | Some ')' ->
            ignore (next p);
            list (List.rev acc)
        | Some '.' when dot_is_delimiter p ->
            if acc = [] then error "dot cannot start a list";
            ignore (next p);
            let tail = parse_datum p in
            skip p;
            expect p ')';
            dotted_list (List.rev acc) tail
        | _ ->
            let datum = parse_datum p in
            elements (datum :: acc)
      in
      elements []

and dot_is_delimiter p =
  p.pos + 1 >= p.len || is_delimiter p.source.[p.pos + 1]

and parse_string p =
  let b = Buffer.create 16 in
  let rec loop () =
    match next p with
    | None -> error "unterminated string"
    | Some '"' -> String (Buffer.contents b)
    | Some '\\' -> (
        match next p with
        | Some 'n' ->
            Buffer.add_char b '\n';
            loop ()
        | Some 'r' ->
            Buffer.add_char b '\r';
            loop ()
        | Some 't' ->
            Buffer.add_char b '\t';
            loop ()
        | Some '"' ->
            Buffer.add_char b '"';
            loop ()
        | Some '\\' ->
            Buffer.add_char b '\\';
            loop ()
        | Some c ->
            Buffer.add_char b c;
            loop ()
        | None -> error "unterminated string escape")
    | Some c ->
        Buffer.add_char b c;
        loop ()
  in
  loop ()

and parse_bar_symbol p =
  let b = Buffer.create 16 in
  let rec loop () =
    match next p with
    | None -> error "unterminated symbol"
    | Some '|' -> Buffer.contents b
    | Some '\\' -> (
        match next p with
        | Some c ->
            Buffer.add_char b c;
            loop ()
        | None -> error "unterminated symbol escape")
    | Some c ->
        Buffer.add_char b c;
        loop ()
  in
  loop ()

and parse_hash p =
  if starts_with p "true" && following_delimiter p 4 then (
    p.pos <- p.pos + 4;
    Bool true)
  else if starts_with p "false" && following_delimiter p 5 then (
    p.pos <- p.pos + 5;
    Bool false)
  else
    match next p with
    | Some ('t' | 'T') ->
        ensure_delimiter p "#t";
        Bool true
    | Some ('f' | 'F') ->
        ensure_delimiter p "#f";
        Bool false
    | Some '\\' -> parse_char p
    | Some '(' -> Vector (parse_vector_items p)
    | Some 'u' when starts_with p "8(" ->
        p.pos <- p.pos + 2;
        Bytevector (parse_bytevector_items p)
    | Some c -> errorf "unsupported # syntax: #%c" c
    | None -> error "unexpected end after #"

and following_delimiter p n =
  p.pos + n >= p.len || is_delimiter p.source.[p.pos + n]

and ensure_delimiter p syntax =
  match peek p with
  | Some c when not (is_delimiter c) ->
      errorf "%s must be followed by a delimiter" syntax
  | _ -> ()

and parse_char p =
  let token = read_token p in
  match String.lowercase_ascii token with
  | "" -> error "empty character literal"
  | "space" -> Char ' '
  | "newline" -> Char '\n'
  | "tab" -> Char '\t'
  | s when String.length s = 1 -> Char s.[0]
  | s -> errorf "unsupported character literal: #\\%s" s

and parse_vector_items p =
  let rec loop acc =
    skip p;
    match peek p with
    | Some ')' ->
        ignore (next p);
        List.rev acc
    | None -> error "unterminated vector"
    | _ -> loop (parse_datum p :: acc)
  in
  loop []

and parse_bytevector_items p =
  let rec loop acc =
    skip p;
    match peek p with
    | Some ')' ->
        ignore (next p);
        List.rev acc
    | None -> error "unterminated bytevector"
    | _ -> (
        match parse_datum p with
        | Number (Int n) when n >= 0 && n <= 255 -> loop (n :: acc)
        | datum ->
            errorf "bytevector item must be an integer in 0..255, got %s"
              (Datum.to_string datum))
  in
  loop []

and parse_atom p first =
  let token = read_token_with_first p first in
  match parse_number token with
  | Some n -> Number n
  | None -> Symbol token

and read_token p =
  match next p with
  | None -> ""
  | Some c -> read_token_with_first p c

and read_token_with_first p first =
  let b = Buffer.create 16 in
  Buffer.add_char b first;
  let rec loop () =
    match peek p with
    | Some c when not (is_delimiter c) ->
        ignore (next p);
        Buffer.add_char b c;
        loop ()
    | _ -> Buffer.contents b
  in
  loop ()

and parse_number token =
  if token = "+" || token = "-" || token = "." then None
  else
    try Some (Int (int_of_string token))
    with Failure _ -> (
      try
        ignore (String.index token '.');
        Some (Float (float_of_string token))
      with Not_found | Failure _ -> (
        try
          ignore (String.index token 'e');
          Some (Float (float_of_string token))
        with Not_found | Failure _ -> (
          try
            ignore (String.index token 'E');
            Some (Float (float_of_string token))
          with Not_found | Failure _ -> None)))

let read_all source =
  let p = of_string source in
  let rec loop acc =
    skip p;
    if eof p then List.rev acc else loop (parse_datum p :: acc)
  in
  loop []

let read_one source =
  match read_all source with
  | [ datum ] -> datum
  | [] -> error "no datum found"
  | _ -> error "expected one datum"
