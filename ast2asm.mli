open X86_64
open Asyntax


(* La fonction qui génère le code assembleur à partir d'un ast *)
val s_of_ast: ast -> string -> unit
