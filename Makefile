all: x86_64.cmo test


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




test: asyntax.cmo test.ml
	ocamlc lexer.cmo asyntax.cmo test.ml -o test




clean:
	rm -rf test *.cmi *.cmo
