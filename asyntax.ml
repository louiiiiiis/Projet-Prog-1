(* Définition du type qui nous permet de représenter un arbre syntaxique *)
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



(* D'abord une fonction qui permet d'extraire un bloc d'instructions entre parenthèses d'une liste de lexemes *)
let extr_brace l =
  let e = ref [] in
  let rec aux i = function
    | []          -> failwith "Erreur d'analyse syntaxique.\nMauvais parenthésage." 
    | Lexer.Lbrace :: t -> e := Lexer.Lbrace :: !e; aux (i + 1) t
    | Lexer.Rbrace :: t -> if i = 1 then t else (e := Lexer.Rbrace :: !e; aux (i - 1) t)
    | h :: t      -> e := h :: !e; aux i t
  in let t = aux 1 l in
  (List.rev !e, t)

(* Et une fonction qui extrait simplement le premier bloc d'instructions d'une liste et renvoie un triplet comportant un indicateur de bloc, le bloc, et le reste de la liste*)
(* L'indicateur de bloc vaut 0 pour un entier, 1 pour un flottant, 2 pour un bloc du type (exp), 3 pour int(exp), 4 pour float(exp), 5 pour +(exp) et 6 pour -(exp) *)
let extr = function
    | []           -> failwith "Erreur d'analyse syntaxique.\nPas d'expression à évaluer"
    | Lexer.Int s :: t   -> (0, [Lexer.Int s], t)
    | Lexer.Float s :: t -> (1, [Lexer.Float s] , t)
    | Lexer.Lbrace :: t  -> let (exp, tt) = extr_brace t in (2, exp, tt)
    | Lexer.Rbrace :: t  -> failwith "Erreur d'analyse syntaxique.\nMauvais parenthésage."
    | Lexer.Iof :: t     -> (match t with
			| Lexer.Lbrace :: tt -> let (exp, ttt) = extr_brace tt in (3, exp, ttt)
			| _                  -> failwith "Erreur d'analyse syntaxique.\nMauvaise utilisation de int, qui doit être suivi par une expression entre parenthèses.")
    | Lexer.Foi :: t     -> (match t with
			| Lexer.Lbrace :: tt -> let (exp, ttt) = extr_brace tt in (4, exp, ttt)
			| _                  -> failwith "Erreur d'analyse syntaxique.\nMauvaise utilisation de float, qui doit être suivi par une expression entre parenthèses.")
    | Lexer.Sum :: t     -> (match t with
			| Lexer.Lbrace :: tt -> let (exp, ttt) = extr_brace tt in (5, exp, ttt)
			| _                  -> failwith "Erreur d'analyse syntaxique.\nUne expression ne peut pas commencer par + si elle n'est pas suivie d'une expression entre parenthèses.")
    | Lexer.Sub :: t     -> (match t with
			| Lexer.Lbrace :: tt -> let (exp, ttt) = extr_brace tt in (6, exp, ttt)
			| _                  -> failwith "Erreur d'analyse syntaxique.\nUne expression ne peut pas commencer par - si elle n'est pas suivie d'une expression entre parenthèses.")
    | Lexer.Mul :: t     -> failwith "Erreur d'analyse syntaxique.\nUne expression ne peut pas commencer par *"
    | Lexer.Div :: t     -> failwith "Erreur d'analyse syntaxique.\nUne expression ne peut pas commencer par /"
    | Lexer.Mod :: t     -> failwith "Erreur d'analyse syntaxique.\nUne expression ne peut pas commencer par %"
    | Lexer.Sumf :: t    -> failwith "Erreur d'analyse syntaxique.\nUne expression ne peut pas commencer par +."
    | Lexer.Subf :: t    -> failwith "Erreur d'analyse syntaxique.\nUne expression ne peut pas commencer par -."
    | Lexer.Mulf :: t    -> failwith "Erreur d'analyse syntaxique.\nUne expression ne peut pas commencer par *."



(* Pour l'analyse lexicale à proprement parler on a besoin de deux fonctions mutuellement récursives *)
(* La première évalue une expression et la seconde évalue une demi-expression commençant par un opérateur et prenant en entrée le sous-arbre gauche de cet opérateur *)
let rec tree l =
  let (i, e, t) = extr l in
  let aux i e = match i with
    | 0 -> (match e with
            | [Lexer.Int s] -> Aint s
            | _       -> failwith "Ce n'est pas censé arriver")
    | 1 -> (match e with
            | [Lexer.Float s] -> Afloat s
            | _       -> failwith "Ce n'est pas censé arriver")
    | 2 -> tree e
    | 3 -> Aiof (tree e)
    | 4 -> Afoi (tree e)
    | 5 -> Apos (tree e)
    | 6 -> Aneg (tree e)
    | _ -> failwith "Ce n'est pas censé arriver"
  in eval (aux i e) t

  and eval ltree l =
    let aux i e = match i with
      | 0 -> (match e with
              | [Lexer.Int s] -> Aint s
              | _       -> failwith "Ce n'est pas censé arriver")
      | 1 -> (match e with
              | [Lexer.Float s] -> Afloat s
              | _       -> failwith "Ce n'est pas censé arriver")
      | 2 -> tree e
      | 3 -> Aiof (tree e)
      | 4 -> Afoi (tree e)
      | 5 -> Apos (tree e)
      | 6 -> Aneg (tree e)
      | _ -> failwith "Ce n'est pas censé arriver"
    in
    match l with
      | []        -> ltree
      | Lexer.Sum :: t  -> Asum (ltree, tree t)
      | Lexer.Sub :: t  -> Asub (ltree, tree t)
      | Lexer.Sumf :: t -> Asumf (ltree, tree t)
      | Lexer.Subf :: t -> Asubf (ltree, tree t)
      | Lexer.Mul :: t  -> let (i, e, tt) = extr t in eval (Amul (ltree, aux i e)) tt
      | Lexer.Div :: t  -> let (i, e, tt) = extr t in eval (Adiv (ltree, aux i e)) tt
      | Lexer.Mod :: t  -> let (i, e, tt) = extr t in eval (Amod (ltree, aux i e)) tt
      | Lexer.Mulf :: t -> let (i, e, tt) = extr t in eval (Amulf (ltree, aux i e)) tt
      | _         -> failwith "Erreur d'analyse syntaxique.\nDeux expressions ne peuvent pas se suivre sans opérateur entre les deux"
























