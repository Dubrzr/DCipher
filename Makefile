CAMLC 		= ocamlopt
CAMLFLAGS 	= -I +sdl -I +site-lib/sdl -g
CAMLLD 		= bigarray.cmxa sdl.cmxa sdlloader.cmxa sdlttf.cmxa sdlgfx.cmxa str.cmxa

SOURCES = utils.ml
SOURCES += matrix.ml
SOURCES += image.ml
SOURCES += convert.ml
SOURCES += treatment.ml
SOURCES += rotation.ml
SOURCES += segm.ml
SOURCES += preproc.ml
SOURCES += neuralnetwork.ml
SOURCES += ocr.ml
SOURCES += main.ml

EXEC 	= dcipher

MLIS = $(SOURCES:.ml=.mli)
OBJS = $(SOURCES:.ml=.cmx)

all: $(EXEC)

mlis: $(MLIS)

$(EXEC): $(OBJS)
	$(CAMLC) $(CAMLFLAGS) $(CAMLLD) -o $@ $^

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.mli:
	$(CAMLC) $(CAMLFLAGS) $(CAMLLD) -i $< > $<i

.ml.cmx:
	$(CAMLC) $(CAMLFLAGS) $(CAMLLD) -c $<

clean::
	rm -f *.o *.cm[iox] *~ .*~ #*#

cleanall:: clean
	rm -f $(EXEC) *.mli
