all: test


lexer.cmo: lexer.ml lexer.cmi
	ocamlc -c lexer.ml
lexer.cmi: lexer.mli
	ocamlc -c lexer.mli


asyntax.cmo: asyntax.ml asyntax.cmi
	ocamlc -c asyntax.ml
asyntax.cmi: lexer.cmo asyntax.mli
	ocamlc -c asyntax.mli


x86_64.cmo: x86_64.ml x86_64.cmi
	ocamlc -c x86_64.ml
x86_64.cmi: x86_64.mli
	ocamlc -c x86_64.mli


ast2asm.cmo: ast2asm.ml ast2asm.cmi
	ocamlc -c ast2asm.ml
ast2asm.cmi: asyntax.cmo x86_64.cmo ast2asm.mli
	ocamlc -c ast2asm.mli




test: lexer.cmo asyntax.cmo x86_64.cmo ast2asm.cmo test.ml
	ocamlc lexer.cmo asyntax.cmo x86_64.cmo ast2asm.cmo test.ml -o test




clean:
	rm -rf test *.cmi *.cmo
