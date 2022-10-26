all: test


lexer.cmo: lexer.ml lexer.cmi
	ocamlc -c lexer.ml

lexer.cmi: lexer.mli
	ocamlc -c lexer.mli


asyntax.cmo: asyntax.ml asyntax.cmi
	ocamlc -c asyntax.ml

asyntax.cmi: asyntax.mli
	ocamlc -c asyntax.mli



test: lexer.cmo asyntax.cmo test.ml
	ocamlc lexer.cmo asyntax.cmo test.ml -o test




clean:
	rm -rf test *.cmi *.cmo
