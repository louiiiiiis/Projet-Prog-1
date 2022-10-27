open Asyntax
open Format
open X86_64


(* Fonction qui génère le texte et la data d'une expression à partir de son ast *)
(* Le texte et la data sont des références qu'on met à jour récursivement *)
(* Pour conserver des valeurs lors des appels récursifs, on utilise la pile de l'assembleur *)
let textndata a =
  let b = ast_ok a in
  let t = ref (globl "main" ++ label "main") in
  let d = ref nop in
  let i = ref 1 in
  let rec aux = function
    | Aint s         -> (t := !t ++ movq (imm (int_of_string s)) (reg rax))
    | Afloat s       -> (t := !t ++ inline ("\tmovsd val" ^ (string_of_int !i) ^ ", %xmm0\n");
                         d := !d ++ label ("val" ^ (string_of_int !i)) ++ (float [Float.of_string s]); incr i)
    | Apos a         -> aux a
    | Aneg a         -> (aux a; t := !t ++ negq (reg rax))
    | Asum (a1, a2)  -> (aux a1; t := !t ++ pushq (reg rax);
                         aux a2; t := !t ++ popq rdi ++ addq (reg rdi) (reg rax))
    | Asub (a1, a2)  -> (aux a2; t := !t ++ pushq (reg rax);
                         aux a1; t := !t ++ popq rdi ++ subq (reg rdi) (reg rax))
    | Amul (a1, a2)  -> (aux a1; t := !t ++ pushq (reg rax);
                         aux a2; t := !t ++ popq rdi ++ imulq (reg rdi) (reg rax))
    | Adiv (a1, a2)  -> (aux a2; t := !t ++ pushq (reg rax);
                         aux a1; t := !t ++ popq rdi ++ movq (imm 0) (reg rdx) ++ idivq (reg rdi))
    | Amod (a1, a2)  -> (aux a2; t := !t ++ pushq (reg rax);
                         aux a1; t := !t ++ popq rdi ++ movq (imm 0) (reg rdx) ++ idivq (reg rdi) ++ movq (reg rdx) (reg rax))
    | Asumf (a1, a2) -> (aux a1; t := !t ++ subq (imm 8) (reg rsp) ++ inline "\tmovsd %xmm0, (%rsp)\n";
                         aux a2; t := !t ++ inline "\tmovsd (%rsp), %xmm1\n" ++ addq (imm 8) (reg rsp) ++ addsd (reg xmm1) (reg xmm0))
    | Asubf (a1, a2) -> (aux a2; t := !t ++ subq (imm 8) (reg rsp) ++ inline "\tmovsd %xmm0, (%rsp)\n";
                         aux a1; t := !t ++ inline "\tmovsd (%rsp), %xmm1\n" ++ addq (imm 8) (reg rsp) ++ subsd (reg xmm1) (reg xmm0))
    | Amulf (a1, a2) -> (aux a1; t := !t ++ subq (imm 8) (reg rsp) ++ inline "\tmovsd %xmm0, (%rsp)\n";
                         aux a2; t := !t ++ inline "\tmovsd (%rsp), %xmm1\n" ++ addq (imm 8) (reg rsp) ++ mulsd (reg xmm1) (reg xmm0))
    | Aiof a         -> (aux a; t := !t ++ cvtsd2si (reg xmm0) (reg rax))
    | Afoi a         -> (aux a; t := !t ++ cvtsi2sd (reg rax) (reg xmm0))
  in aux a;
  if b then (t := !t ++ call "print_int"
		     ++ ret
		     ++ label "print_int"
		     ++ movq (reg rax) (reg rsi)
                     ++ inline "\tmovq $message_int, %rdi\n"
		     ++ movq (imm 0) (reg rax)
                     ++ call "printf"
                     ++ ret;
             d := !d ++ label "message_int"
                     ++ string "%d\n")
       else (t := !t ++ call "print_float"
		     ++ ret
                     ++ label "print_float"
                     ++ inline "\tmovq $message_float, %rdi\n"
		     ++ movq (imm 1) (reg rax)
                     ++ call "printf"
                     ++ ret;
             d := !d ++ label "message_float"
                     ++ string "%f\n");
  (!t, !d)


(* Fonction qui fabrique le fichier assembleur à partir d'un ast et du nom du fichier à créer *)
(* On utilise ici simplement les fonctions des modules Format et X86_64 *)
let s_of_ast a name =
  let (t, d) = textndata a in
  let code = { text = t;
               data = d; } in
  let c = open_out (name ^ ".s") in
  let fmt = formatter_of_out_channel c in
  X86_64.print_program fmt code;
  close_out c



















