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


(* Une erreur d'analyse lexicale apparaît lorsqu'un caractère ou chaîne de caractères inconnue est renseignée en entrée *)
exception LexError of char


(* La fonction d'analyse lexicale qui transforme une chaîne de caractères en une liste de lexemes *)
val lexeme_list_of_str : string -> lexeme list
