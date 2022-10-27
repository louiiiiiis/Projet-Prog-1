open Asyntax
open Format
open X86_64


(* Fonction qui génère le texte et la data d'une expression à partir de son ast *)
let textndata a =
  let b = ast_ok a in
  let t = ref (globl "main" ++ label "main") in
  let d = ref nop in
  let rec aux = function
    | Aint s         -> (t := !t ++ movq (imm (int_of_string s)) (reg rax))
    | Afloat s       -> (t := !t ++ movsd (reg rbx) (reg xmm0);
                         d := !d ++ label "aaa" ++ (float [Float.of_string s]))
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
    | Asumf (a1, a2) -> (aux a1; t := !t ++ pushq (reg xmm0);
                         aux a2; t := !t ++ popq xmm1 ++ addsd (reg xmm1) (reg xmm0))
    | Asubf (a1, a2) -> (aux a2; t := !t ++ pushq (reg xmm0);
                         aux a1; t := !t ++ popq xmm1 ++ subsd (reg xmm1) (reg xmm0))
    | Amulf (a1, a2) -> (aux a1; t := !t ++ pushq (reg xmm0);
                         aux a2; t := !t ++ popq xmm1 ++ mulsd (reg xmm1) (reg xmm0))
    | Aiof a         -> (aux a; t := !t ++ cvtsd2si (reg xmm0) (reg rax))
    | Afoi a         -> (aux a; t := !t ++ cvtsi2sd (reg rax) (reg xmm0))
  in aux a;
  if b then (t := !t ++ movq (reg rax) (reg rsi)
                     ++ inline "movq $message_int, %rdi"
		     ++ movq (imm 0) (reg rax)
                     ++ call "printf"
                     ++ ret;
             d := !d ++ label "message_int"
                     ++ string "%d\n")
       else (t := !t ++ inline "movq $message_float, %rdi"
		     ++ movq (imm 1) (reg rax)
                     ++ call "printf"
                     ++ ret;
             d := !d ++ label "message_float"
                     ++ string "%f\n");
  (!t, !d)


(* Fonction qui fabrique le fichier assembleur à partir d'un ast et du nom du fichier à créer *)
let s_of_ast a name =
  let (t, d) = textndata a in
  let code = { text = t;
               data = d; } in
  let c = open_out (name ^ ".s") in
  let fmt = formatter_of_out_channel c in
  X86_64.print_program fmt code;
  close_out c



















