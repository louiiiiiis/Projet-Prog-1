open X86_64
open Asyntax


(* La fonction qui génère le code assembleur *)
val s_of_ast: ast -> string -> unit
