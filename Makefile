all: main rapport.pdf

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


main: lexer.cmo asyntax.cmo x86_64.cmo ast2asm.cmo main.ml
	ocamlc lexer.cmo asyntax.cmo x86_64.cmo ast2asm.cmo main.ml -o aritha


rapport.pdf: rapport.tex
	pdflatex -shell-escape rapport.tex
	rm -rf _minted-rapport *.aux *.log *.out


clean:
	rm -rf aritha *.cmi *.cmo *.s
	rm -rf _minted-rapport *.pdf *.aux *.log *.out






test: lexer.cmo asyntax.cmo x86_64.cmo ast2asm.cmo test.ml
	ocamlc lexer.cmo asyntax.cmo x86_64.cmo ast2asm.cmo test.ml -o test

compile_tests: t1 t2 t3 t4 t5

t1: t1.s
	gcc -no-pie t1.s -o t1

t2: t2.s
	gcc -no-pie t2.s -o t2

t3: t3.s
	gcc -no-pie t3.s -o t3

t4: t4.s
	gcc -no-pie t4.s -o t4

t5: t5.s
	gcc -no-pie t5.s -o t5

clean_test:
	rm -rf test *.s t1 t2 t3 t4 t5
