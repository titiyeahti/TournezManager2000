try : chemin try.ml
	ocamlc -c try.ml

parser : chemin parser.ml lexer.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml

chemin : chemin.ml
	ocamlc -c chemin.ml

parser.ml : parser.mly
	ocamlyacc parser.mly

lexer.ml : lexer.mll
	ocamllex lexer.mll

clean : 
	(rm -rf _build;	rm *.native; rm *.cmi; rm *.cmo;)

