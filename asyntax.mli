open Lexer

(* Définition du type ast *)
type ast =
  | Aint of string
  | Afloat of string
  | Apos of ast
  | Aneg of ast
  | Asum of ast * ast
  | Asub of ast * ast
  | Amul of ast * ast
  | Adiv of ast * ast
  | Amod of ast * ast
  | Asumf of ast * ast
  | Asubf of ast * ast
  | Amulf of ast * ast
  | Aiof of ast
  | Afoi of ast


(* La fonction d'analyse syntaxique qui transforme une liste de lexemes en arbre syntaxique *)
val tree : lexeme list -> ast


(* Fonction qui vérifie le bon typage d'un arbre *)
val ast_ok : ast -> bool
