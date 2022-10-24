all: test

lexer.cmo: lexer.ml lexer.cmi
	ocamlc -c lexer.ml

lexer.cmi: lexer.mli
	ocamlc -c lexer.mli

test: lexer.cmo test.ml
	ocamlc lexer.cmo test.ml -o test




clean:
	rm -rf test *.cmi *.cmo
