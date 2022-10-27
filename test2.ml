open Format
open X86_64


let t = ref (globl "main" ++ label "main")
let d = ref nop

let () = t := !t ++ movq (imm 15) (reg xmm0) ++ pushq (reg xmm0) ++ popq xmm1 ++ inline "
"
let () = d := !d ++ label "message" ++ string "aaaaaaaaaa" ++ int [15] ++ float [12.0]

let code = { text = !t; data = !d }
let c = open_out "aaaaaaa.s"
let fmt = formatter_of_out_channel c

let () = print_program fmt code; close_out c
