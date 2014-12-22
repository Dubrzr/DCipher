OCAMLC=ocamlopt -g
LD=-I ./src/ -I +sdl -I +lablgtk2 bigarray.cmxa sdl.cmxa sdlgfx.cmxa sdlloader.cmxa sdlttf.cmxa lablgtk.cmxa gtkInit.cmx lablgtkspell.cmxa unix.cmxa

SRC=src/utils.ml
SRC+=src/matrix.ml
SRC+=src/image.ml
SRC+=src/convert.ml
SRC+=src/treatment.ml
SRC+=src/rotation.ml
SRC+=src/segm.ml
SRC+=src/preproc.ml
SRC+=src/serialization.ml
SRC+=src/neuralnetwork.ml
SRC+=src/ocr.ml
SRC+=src/gui.ml
SRC+=src/main.ml

EXEC=dcipher

OBJ=$(SRC:.ml=.cmx)

all: $(MLI) $(EXEC)

$(EXEC): $(OBJ)
	$(OCAMLC) $(LD) -o $(EXEC) $^

.SUFFIXES: .ml .mli .cmx .cmi

.ml.cmx:
	$(OCAMLC) $(LD) -c $<

clean:
	rm -f src/*.{cm?,o}
