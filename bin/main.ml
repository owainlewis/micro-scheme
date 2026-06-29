open Micro_scheme

let print_value value =
  match value with
  | Value.Void -> ()
  | _ -> print_endline (Value.to_string value)

let run_expr expr =
  let env = Run.initial_env () in
  print_value (Run.eval_string env expr)

let run_file env path =
  print_value (Run.eval_file env path)

let repl () =
  let env = Run.initial_env () in
  let rec loop () =
    print_string "micro-scheme> ";
    flush stdout;
    match read_line () with
    | exception End_of_file -> print_newline ()
    | line -> (
        try
          print_value (Run.eval_string env line);
          loop ()
        with
        | Error.Error message ->
            prerr_endline ("error: " ^ message);
            loop ())
  in
  loop ()

let usage () =
  prerr_endline "usage: micro-scheme [-e expr] [file ...]";
  exit 2

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> repl ()
  | [ _; "-e"; expr ] -> run_expr expr
  | _ :: "-e" :: _ -> usage ()
  | _ :: files ->
      let env = Run.initial_env () in
      List.iter (run_file env) files
  | [] -> usage ()
