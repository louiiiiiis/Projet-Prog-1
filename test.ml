open Lexer

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

let affiche l =
  let s = ref "" in
  let rec aux = function
    | []      -> ()
    | t :: q  -> s := (str_of_lexeme t) ^ ", " ^ !s; aux q
  in aux (List.rev l);
  s := "[" ^ (String.sub !s 0 (String.length !s - 2)) ^ "]\n";
  print_string !s



let t1 = "5"
let t2 = "5+2"
let t3 = "5 /3% 4"
let t4 = "524 +. 22.01"
let t5 = "5. * - int(5.)"

let lt1 = lexeme_list_of_str t1
let lt2 = lexeme_list_of_str t2
let lt3 = lexeme_list_of_str t3
let lt4 = lexeme_list_of_str t4
let lt5 = lexeme_list_of_str t5


let () = (print_string (t1 ^ " -> "); (affiche lt1);
          print_string (t2 ^ " -> "); (affiche lt2);
          print_string (t3 ^ " -> "); (affiche lt3);
          print_string (t4 ^ " -> "); (affiche lt4);
          print_string (t5 ^ " -> "); (affiche lt5))


