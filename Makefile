OCAML=ocamlopt
OCAMLFLAGS= -I /usr/lib/ocaml/site-lib/sdl
OCAMLLD=bigarray.cmxa sdl.cmxa sdlloader.cmxa

test: test.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o test test.ml

clean:
	rm -rf *~ *.o *.cm? test
