(* Définition du type lexeme *)
type lexeme =
  | Int of string
  | Float of string
  | Lbrace
  | Rbrace
  | Iof
  | Foi
  | Sum
  | Sub
  | Mul
  | Div
  | Mod
  | Sumf
  | Subf
  | Mulf


(* La fonction d'analyse lexicale qui transforme une chaîne de caractères en une liste de lexemes *)
val lexeme_list_of_str : string -> lexeme list
