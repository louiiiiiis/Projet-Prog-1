open Lexer

(* Définition du type ast pour représenter les arbres syntaxiques *)
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
    | []          -> failwith "\nErreur d'analyse syntaxique.\nMauvais parenthésage." 
    | Lbrace :: t -> e := Lbrace :: !e; aux (i + 1) t
    | Rbrace :: t -> if i = 1 then t else (e := Rbrace :: !e; aux (i - 1) t)
    | h :: t      -> e := h :: !e; aux i t
  in let t = aux 1 l in
  (List.rev !e, t)

(* Et une fonction qui extrait simplement le premier bloc d'instructions d'une liste et renvoie un triplet comportant un indicateur de bloc, le bloc, et le reste de la liste*)
(* L'indicateur de bloc vaut 0 pour un entier, 1 pour un flottant, 2 pour un bloc du type (exp), 3 pour int(exp), 4 pour float(exp), 5 pour +(exp) et 6 pour -(exp) *)
let extr = function
    | []           -> failwith "\nErreur d'analyse syntaxique.\nPas d'expression à évaluer"
    | Int s :: t   -> (0, [Int s], t)
    | Float s :: t -> (1, [Float s] , t)
    | Lbrace :: t  -> let (exp, tt) = extr_brace t in (2, exp, tt)
    | Rbrace :: t  -> failwith "\nErreur d'analyse syntaxique.\nMauvais parenthésage."
    | Iof :: t     -> (match t with
			| Lbrace :: tt -> let (exp, ttt) = extr_brace tt in (3, exp, ttt)
			| _            -> failwith "\nErreur d'analyse syntaxique.\nMauvaise utilisation de int, qui doit être suivi par une expression entre parenthèses.")
    | Foi :: t     -> (match t with
			| Lbrace :: tt -> let (exp, ttt) = extr_brace tt in (4, exp, ttt)
			| _            -> failwith "\nErreur d'analyse syntaxique.\nMauvaise utilisation de float, qui doit être suivi par une expression entre parenthèses.")
    | Sum :: t     -> (match t with
			| Int s :: tt   -> (0, [Int s], tt)
                        | Float s :: tt -> (1, [Float s], tt)
                        | Lbrace :: tt  -> let (exp, ttt) = extr_brace tt in (5, exp, ttt)
			| _             -> failwith "\nErreur d'analyse syntaxique.\nUne expression ne peut pas commencer par + si elle n'est pas suivie d'un nombre ou d'une expression entre parenthèses.")
    | Sub :: t     -> (match t with
			| Int s :: tt   -> (0, [Int ("-" ^ s)], tt)
                        | Float s :: tt -> (1, [Float ("-" ^ s)], tt)
			| Lbrace :: tt  -> let (exp, ttt) = extr_brace tt in (6, exp, ttt)
			| _             -> failwith "\nErreur d'analyse syntaxique.\nUne expression ne peut pas commencer par - si elle n'est pas suivie d'un nombre ou d'une expression entre parenthèses.")
    | Mul :: t     -> failwith "\nErreur d'analyse syntaxique.\nUne expression ne peut pas commencer par \"*\"."
    | Div :: t     -> failwith "\nErreur d'analyse syntaxique.\nUne expression ne peut pas commencer par \"/\"."
    | Mod :: t     -> failwith "\nErreur d'analyse syntaxique.\nUne expression ne peut pas commencer par \"%\"."
    | Sumf :: t    -> failwith "\nErreur d'analyse syntaxique.\nUne expression ne peut pas commencer par \"+.\"."
    | Subf :: t    -> failwith "\nErreur d'analyse syntaxique.\nUne expression ne peut pas commencer par \"-.\"."
    | Mulf :: t    -> failwith "\nErreur d'analyse syntaxique.\nUne expression ne peut pas commencer par \"*.\"."



(* Pour l'analyse syntaxique à proprement parler on a besoin de deux fonctions mutuellement récursives *)
(* La première évalue une expression et la seconde évalue une demi-expression commençant par un opérateur, elle prend également en entrée le sous-arbre gauche de cet opérateur *)
(* Pour évaluer une expression on évalue récursivement son premier bloc en formant un sous-arbre gauche puis on évalue la demi-expression restante *)
(* Pour évaluer une demi-expression on distingue les cas selon l'opérateur afin de respecter les priorités d'opération, puis on évalue récursivement ce qu'il reste à droite *)
let rec tree l =
  let (i, e, t) = extr l in
  let aux i e = match i with
    | 0 -> (match e with
            | [Int s] -> Aint s
            | _       -> failwith "Ce n'est pas censé arriver")
    | 1 -> (match e with
            | [Float s] -> Afloat s
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
              | [Int s] -> Aint s
              | _       -> failwith "Ce n'est pas censé arriver")
      | 1 -> (match e with
              | [Float s] -> Afloat s
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
      | Sum :: t  -> Asum (ltree, tree t)
      | Sub :: t  -> Asub (ltree, tree t)
      | Sumf :: t -> Asumf (ltree, tree t)
      | Subf :: t -> Asubf (ltree, tree t)
      | Mul :: t  -> let (i, e, tt) = extr t in eval (Amul (ltree, aux i e)) tt
      | Div :: t  -> let (i, e, tt) = extr t in eval (Adiv (ltree, aux i e)) tt
      | Mod :: t  -> let (i, e, tt) = extr t in eval (Amod (ltree, aux i e)) tt
      | Mulf :: t -> let (i, e, tt) = extr t in eval (Amulf (ltree, aux i e)) tt
      | _         -> failwith "\nErreur d'analyse syntaxique.\nDeux expressions ne peuvent pas se suivre sans opérateur entre les deux."


(* Fonction qui teste le typage d'un arbre, déclenche une erreur si il est mauvais, renvoie true pour un entier et false pour un flottant *)
let rec ast_ok =  function
    | Aint s         -> true
    | Afloat s       -> false
    | Apos a         -> if (ast_ok a) then true else failwith "\nErreur d'analyse syntaxique.\nMauvais typage : +(expression) ne peut être appliqué qu'à une expression entière."
    | Aneg a         -> if (ast_ok a) then true else failwith "\nErreur d'analyse syntaxique.\nMauvais typage : -(expression) ne peut être appliqué qu'à une expression entière."
    | Asum (a1, a2)  -> if (ast_ok a1) && (ast_ok a2) then true else failwith "\nErreur d'analyse syntaxique.\nMauvais typage : (expression1) + (expression2) ne peut être appliqué qu'à des expressions entières."
    | Asub (a1, a2)  -> if (ast_ok a1) && (ast_ok a2) then true else failwith "\nErreur d'analyse syntaxique.\nMauvais typage : (expression1) - (expression2) ne peut être appliqué qu'à des expressions entières."
    | Amul (a1, a2)  -> if (ast_ok a1) && (ast_ok a2) then true else failwith "\nErreur d'analyse syntaxique.\nMauvais typage : (expression1) * (expression2) ne peut être appliqué qu'à des expressions entières."
    | Adiv (a1, a2)  -> if (ast_ok a1) && (ast_ok a2) then true else failwith "\nErreur d'analyse syntaxique.\nMauvais typage : (expression1) / (expression2) ne peut être appliqué qu'à des expressions entières;"
    | Amod (a1, a2)  -> if (ast_ok a1) && (ast_ok a2) then true else failwith "\nErreur d'analyse syntaxique.\nMauvais typage : (expression1) % (expression2) ne peut être appliqué qu'à des expressions entières."
    | Asumf (a1, a2) -> if (not (ast_ok a1)) && (not (ast_ok a2)) then false else failwith "\nErreur d'analyse syntaxique.\nMauvais typage : (expression1) +. (expression2) ne peut être appliqué qu'à des expressions flottantes."
    | Asubf (a1, a2) -> if (not (ast_ok a1)) && (not (ast_ok a2)) then false else failwith "\nErreur d'analyse syntaxique.\nMauvais typage : (expression1) -. (expression2) ne peut être appliqué qu'à des expressions flottantes."
    | Amulf (a1, a2) -> if (not (ast_ok a1)) && (not (ast_ok a2)) then false else failwith "\nErreur d'analyse syntaxique.\nMauvais typage : (expression1) *. (expression2) ne peut être appliqué qu'à des expressions flottantes."
    | Aiof a         -> if not (ast_ok a) then true else failwith "\nErreur d'analyse syntaxique.\nMauvais typage : int(expression) ne peut être appliqué qu'à une expression flottante."
    | Afoi a         -> if (ast_ok a) then false else failwith "\nErreur d'analyse syntaxique.\nMauvais typage : float(expression) ne peut être appliqué qu'à une expression entière."















