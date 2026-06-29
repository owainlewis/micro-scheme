exception Error of string

let error message = raise (Error message)
let errorf fmt = Printf.ksprintf error fmt
