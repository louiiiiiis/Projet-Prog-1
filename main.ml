open Lexer
open Asyntax
open Format
open x86_64


(* On récupère l'expression en entrée *)
let input = "5 + 3 * 2"


(* On construit notre AST bien typé grâce aux modules Lexer et Asyntax *)
let lexeme_list = lexeme_list_of_str input
let ast = ast lexeme_list
let tast = tast ast



(* On écrit l'AST dans un fichier assembleur qui a le même nom que le fichier en entrée *)
let () =
  let code = {
    text =
      globl "main" ++
      label "main" ++
      movq (reg rax) (reg rdi) ++
      call "print_int" ++
      ret ++

      inline "
print_int:
	movq %rdi %rsi
	movq $S_int, %rdi
	xorq %rax, %rax
	call printf
	ret
";

    data =
      label "S_int" ++
      string "%d\n";
  } in
  let c = open_out "out.s" in
  let fmt = formatter_of_out_channel c in
  X86_64.print_program fmt code;
  close_out c



