let initial_env = Eval.initial_env
let eval_string = Eval.eval_string
let eval_file = Eval.eval_file

let eval_to_string source =
  let env = initial_env () in
  Value.to_string (eval_string env source)
