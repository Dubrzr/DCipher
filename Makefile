OCAML 		= ocamlopt
OCAMLFLAGS 	= -I +sdl -I +site-lib/sdl -thread
OCAMLLD 	= bigarray.cmxa sdl.cmxa sdlgfx.cmxa sdlloader.cmxa sdlttf.cmxa
OCAMLLD		+= unix.cmxa threads.cmxa
CC 		= ${OCAML} ${OCAMLFLAGS} ${OCAMLLD}

FILES 	= utils.cmx
FILES 	+= image.cmx
FILES 	+= matrix.cmx
FILES	+= preprocessing.cmx
FILES	+= charsfinding.cmx
FILES	+= charsrecognition.cmx
FILES	+= ocr.cmx
FILES 	+= main.cmx

all: dcipher

dcipher: ${FILES}
	${CC} -o $@ $^

%.cmx: %.ml
	${CC} -c $<

clean:
	rm -f *~ *.o *.cm?
