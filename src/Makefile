test : try.cmo
	ocamlc -o test graphics.cma chemin.cmo try.cmo
	./test

try.cmo : chemin.cmi try.ml
	ocamlc -c try.ml

main : main.cmo chemin.cmo
	ocamlc -o exec graphics.cma chemin.cmo parse.cmo main.cmo 

main.cmo : main.ml parse.cmo chemin.cmi
	ocamlc -c main.ml

parse.cmo : parse.ml 
	ocamlc -c parse.ml

parser : chemin.cmo parser.ml lexer.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml

chemin.cmo : chemin.ml chemin.cmi
	ocamlc -c chemin.ml

chemin.cmi : chemin.mli
	ocamlc -c chemin.mli

parser.ml : parser.mly
	ocamlyacc parser.mly

lexer.ml : lexer.mll
	ocamllex lexer.mll

clean : 
	(rm -rf _build;	rm *.native; rm *.cmi; rm *.cmo; rm test; rm exec)

