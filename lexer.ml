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


(* D'abord une fonction qui teste si un caractère est un entier ou pas *)
let is_int c =
  (int_of_char c >= int_of_char '0') && (int_of_char c <= int_of_char '9')


(* La fonction d'analyse lexicale qui transforme une chaîne de caractères en une liste de lexemes *)
(* On regarde simplement les caractères un par un pour les transformer en lexemes *)
let lexeme_list_of_str s =
  let n = String.length s in
  let l = ref [] in
  let i = ref 0 in
  while !i < n do
    try 
      if s.[!i] = ' ' then (incr i) else
      if s.[!i] = '(' then (l := Lbrace :: !l; incr i) else
      if s.[!i] = ')' then (l := Rbrace :: !l; incr i) else
      if s.[!i] = '+' then (incr i; if (!i < n && s.[!i] = '.') then (l := Sumf :: !l; incr i)
                                                                else (l := Sum :: !l)) else
      if s.[!i] = '-' then (incr i; if (!i < n && s.[!i] = '.') then (l := Subf :: !l; incr i)
                                                                else (l := Sub :: !l)) else
      if s.[!i] = '*' then (incr i; if (!i < n && s.[!i] = '.') then (l := Mulf :: !l; incr i)
                                                                else (l := Mul :: !l)) else
      if s.[!i] = '/' then (l := Div :: !l; incr i) else
      if s.[!i] = '%' then (l := Mod :: !l; incr i) else
      if ((!i + 2) < n && String.equal (String.sub s !i 3) "int")   then (l := Iof :: !l; i := !i + 3) else
      if ((!i + 4) < n && String.equal (String.sub s !i 5) "float") then (l := Foi :: !l; i := !i + 5) else
      if (is_int s.[!i]) then
        begin
	  let j = ref 1 in
	  (while ((!i + !j < n) && (is_int s.[!i + !j])) do incr j done;
	  if ((!i + !j < n) && (s.[!i + !j] = '.')) then 
            (incr j; 
            while ((!i + !j < n) && (is_int s.[!i + !j])) do incr j done; 
            l := Float(String.sub s !i !j) :: !l; i := !i + !j)
	  else 
            (l := Int(String.sub s !i !j) :: !l; i := !i + !j))
	end 
      else
        raise (LexError s.[!i])
    with LexError c -> failwith ("\nErreur d'analyse lexicale.\nCaractère invalide : \"" ^ (Char.escaped c) ^ "\".")
  done;
  List.rev !l










