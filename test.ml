open Lexer
open Asyntax


(* Fontion qui transorme une lexeme en string *)
let str_of_lexeme = function
  | Int s   -> s   
  | Float s -> s
  | Lbrace  -> "Lbrace"
  | Rbrace  -> "Rbrace"
  | Iof     -> "Iof"
  | Foi     -> "Foi"
  | Sum     -> "Sum"
  | Sub     -> "Sub"
  | Mul     -> "Mul"
  | Div     -> "Div"
  | Mod     -> "Mod"
  | Sumf    -> "Sumf"
  | Subf    -> "Subf"
  | Mulf    -> "Mulf"

(* Fonction qui affiche une liste de lexemes *)
let affiche_lexlist l =
  let s = ref "" in
  let rec aux = function
    | []      -> ()
    | t :: q  -> s := (str_of_lexeme t) ^ ", " ^ !s; aux q
  in aux (List.rev l);
  s := "[" ^ (String.sub !s 0 (String.length !s - 2)) ^ "]";
  print_string !s


(* Les valeurs de test *)
let t1 = "5"
let t2 = "5+2"
let t3 = "5 /3% 4"
let t4 = "524 +. 22.01"
let t5 = "5. * - int(5.)"

(* Transformation des tests en listes de lexemes *)
let lt1 = lexeme_list_of_str t1
let lt2 = lexeme_list_of_str t2
let lt3 = lexeme_list_of_str t3
let lt4 = lexeme_list_of_str t4
let lt5 = lexeme_list_of_str t5


(* Transformation des listes de lexemes en arbres syntaxiques *)
let at1 = tree lt1
let at2 = tree lt2
let at3 = tree lt3
let at4 = tree lt5
let at5 = tree lt5


(* Affichage *)
let () = (print_string (t1 ^ " -> "); (affiche_lexlist lt1); print_string " -> \n";
          print_string (t2 ^ " -> "); (affiche_lexlist lt2); print_string " -> \n";
          print_string (t3 ^ " -> "); (affiche_lexlist lt3); print_string " -> \n";
          print_string (t4 ^ " -> "); (affiche_lexlist lt4); print_string " -> \n";
          print_string (t5 ^ " -> "); (affiche_lexlist lt5); print_string " -> \n")


