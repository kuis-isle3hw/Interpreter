open Miniml.Cui
open Miniml.Syntax
open Miniml.Eval
open Miniml.Environment

let my_env =
  extend "ii" (IntV 2)
    (extend "iii" (IntV 3)
      (extend "iv" (IntV 4) initial_env))

(* New! initial_tyenv を REPL の最初の呼び出しで渡す *)
let _ = read_eval_print initial_env initial_tyenv 


