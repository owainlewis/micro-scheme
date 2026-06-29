let known_libraries =
  [
    [ "scheme"; "base" ];
    [ "scheme"; "write" ];
    [ "scheme"; "read" ];
    [ "scheme"; "repl" ];
    [ "scheme"; "load" ];
    [ "scheme"; "process-context" ];
    [ "scheme"; "time" ];
    [ "scheme"; "char" ];
    [ "scheme"; "cxr" ];
  ]

let is_known name = List.exists (( = ) name) known_libraries
