test : try.cmo
	ocamlc -o test graphics.cma chemin.cmo try.cmo
	./test

try.cmo : chemin.cmi try.ml
	ocamlc -c try.ml

main : chemin.ml parse.ml main.ml
	ocamlopt -o main.native graphics.cmxa chemin.mli chemin.ml parse.ml main.ml

clean : 
	(rm -rf _build;	rm *.native; rm *.cmi; rm *.cmo; rm test; rm exec; rm *.o)

