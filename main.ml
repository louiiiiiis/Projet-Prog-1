open Lexer
open Asyntax
open Format
open X86_64
open Ast2asm


(* On récupère le nom du fichier donnée en entrée *)
let file = read_line ()
let () = if not (String.equal (String.sub file ((String.length file) - 4) 4) ".exp") then failwith "\nFichier en entrée invalide.\nUne extention \".exp\" est attendue."
let file_name = String.sub file 0 ((String.length file) - 4)

(* On récupère ce qui est écrit dans le fichier *)
let fileop = open_in file
let input = input_line fileop


(* On construit notre liste de lexemes puis notre AST, et on vérifie qu'il est bien typé *)
let lexeme_list = lexeme_list_of_str input
let ast = tree lexeme_list
let b = ast_ok ast


(* Finalement on génère le code assembleur *)
let () = s_of_ast ast file_name
let () = print_string "Compilation effectuée avec succès.\n"
let () = print_string ("Le fichier " ^ file_name ^ ".s peut être compilé avec la commande \"gc -no-pie " ^ file_name ^ ".s -o " ^ file_name ^ "\".\n")
