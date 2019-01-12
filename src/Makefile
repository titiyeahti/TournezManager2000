all : main

test : main test.sh
	./test.sh

main : chemin.ml parse.ml main.ml
	ocamlopt -o main.native graphics.cmxa chemin.mli chemin.ml parse.ml main.ml

clean : 
	(rm -rf _build;	rm *.native; rm *.cmi; rm *.cmo; rm test; rm exec; rm *.o; rm *.cmx)

