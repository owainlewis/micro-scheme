open Micro_scheme

let failures = ref []

let check name expected actual =
  if expected <> actual then
    failures := (name, expected, actual) :: !failures

let check_eval name expected source =
  try check name expected (Run.eval_to_string source)
  with Error.Error message -> check name expected ("error: " ^ message)

let check_reader name expected source =
  try check name expected (Datum.to_string (Reader.read_one source))
  with Error.Error message -> check name expected ("error: " ^ message)

let check_reader_error name source =
  try
    ignore (Reader.read_one source);
    check name "error" "ok"
  with Error.Error _ -> ()

let check_error name source =
  try
    ignore (Run.eval_to_string source);
    check name "error" "ok"
  with Error.Error _ -> ()

let reader_tests () =
  check_reader "boolean true" "#t" "#true";
  check_reader "integer" "42" "42";
  check_reader "float" "3.5" "3.5";
  check_reader "char" "#\\space" "#\\space";
  check_reader "string escapes" "\"a\\n\"" "\"a\\n\"";
  check_reader "symbol" "hello-world!" "hello-world!";
  check_reader "proper list" "(a b c)" "(a b c)";
  check_reader "dotted list" "(a b . c)" "(a b . c)";
  check_reader "vector" "#(1 #t \"x\")" "#(1 #t \"x\")";
  check_reader "bytevector" "#u8(1 2 255)" "#u8(1 2 255)";
  check_reader "quote syntax" "(quote a)" "'a";
  check_reader "quasiquote syntax" "(quasiquote (a (unquote b)))" "`(a ,b)";
  check_reader "line comment" "(a b)" "; ignored\n(a b)";
  check_reader "datum comment" "(a c)" "(a #;b c)";
  check_reader "block comment" "(a c)" "(a #| b |# c)";
  check_reader_error "boolean needs delimiter" "#truex";
  check_reader_error "dot cannot start list" "(. a)"

let eval_tests () =
  check_eval "literal number" "7" "7";
  check_eval "quote" "(a b c)" "'(a b c)";
  check_eval "define and reference" "9" "(define x 9) x";
  check_eval "set bang" "4" "(define x 1) (set! x 4) x";
  check_eval "if true" "1" "(if #t 1 2)";
  check_eval "begin" "3" "(begin 1 2 3)";
  check_eval "lambda application" "7" "((lambda (x) (+ x 2)) 5)";
  check_eval "closure captures lexical env" "12"
    "(define make-adder (lambda (x) (lambda (y) (+ x y)))) ((make-adder 5) 7)";
  check_eval "variadic lambda" "(2 3)" "((lambda (x . rest) rest) 1 2 3)";
  check_eval "function define shorthand" "25" "(define (square x) (* x x)) (square 5)";
  check_eval "let" "7" "(let ((x 3) (y 4)) (+ x y))";
  check_eval "named let tail recursion" "55"
    "(let loop ((n 10) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc n))))";
  check_eval "let star" "8" "(let* ((x 3) (y (+ x 5))) y)";
  check_eval "letrec" "120"
    "(letrec ((fact (lambda (n acc) (if (= n 0) acc (fact (- n 1) (* acc n)))))) (fact 5 1))";
  check_eval "and" "#f" "(and #t 1 #f 3)";
  check_eval "or returns value" "7" "(or #f 7 9)";
  check_eval "cond" "large" "(cond ((< 3 2) 'small) ((> 3 2) 'large) (else 'none))";
  check_eval "case" "vowel" "(case 'a ((a e i o u) 'vowel) (else 'other))";
  check_eval "when" "9" "(define x 1) (when #t (set! x 9)) x";
  check_eval "unless" "9" "(define x 1) (unless #f (set! x 9)) x";
  check_eval "delay and force" "7" "(define x 1) (define p (delay (+ x 6))) (force p)";
  check_eval "quasiquote" "(a 3 4 5)" "(define xs '(4 5)) `(a ,(+ 1 2) ,@xs)";
  check_eval "do" "10"
    "(do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((> i 4) sum))";
  check_error "unbound variable error" "missing-name"

let primitive_tests () =
  check_eval "arithmetic" "15" "(+ 1 2 3 4 5)";
  check_eval "comparison" "#t" "(<= 1 2 2 3)";
  check_eval "pair operations" "(1 . 9)" "(define p (cons 1 2)) (set-cdr! p 9) p";
  check_eval "list length" "3" "(length '(a b c))";
  check_eval "append" "(1 2 3 4)" "(append '(1 2) '(3) '(4))";
  check_eval "string operations" "#\\c" "(string-ref (string-append \"ab\" \"cd\") 2)";
  check_eval "symbol conversion" "abc" "(string->symbol (symbol->string 'abc))";
  check_eval "vector operations" "9"
    "(define v (vector 1 2 3)) (vector-set! v 1 9) (vector-ref v 1)";
  check_eval "vector conversion" "(1 2)" "(vector->list (list->vector '(1 2)))";
  check_eval "bytevector operations" "255"
    "(define b (make-bytevector 2 0)) (bytevector-u8-set! b 1 255) (bytevector-u8-ref b 1)";
  check_eval "equal nested data" "#t" "(equal? '(1 (2 3)) '(1 (2 3)))"

let conformance_tests () =
  check_eval "import scheme base no op" "3" "(import (scheme base)) (+ 1 2)";
  check_eval "deep tail recursion" "20000"
    "(define (loop n acc) (if (= n 0) acc (loop (- n 1) (+ acc 1)))) (loop 20000 0)"

let example_tests () =
  let env = Run.initial_env () in
  let example_path name =
    let candidates = [ "examples/" ^ name; "../examples/" ^ name ] in
    match List.find_opt Sys.file_exists candidates with
    | Some path -> path
    | None -> failwith ("missing example: " ^ name)
  in
  let run path = Value.to_string (Run.eval_file env path) in
  check "factorial example" "720" (run (example_path "factorial.scm"));
  check "lists example" "(1 2 3 4 5)" (run (example_path "lists.scm"));
  check "vectors example" "42" (run (example_path "vectors.scm"))

let () =
  reader_tests ();
  eval_tests ();
  primitive_tests ();
  conformance_tests ();
  example_tests ();
  match List.rev !failures with
  | [] -> print_endline "all tests passed"
  | failures ->
      List.iter
        (fun (name, expected, actual) ->
          Printf.eprintf "FAIL %s\nexpected: %s\nactual:   %s\n" name expected
            actual)
        failures;
      exit 1
