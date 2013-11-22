CAMLC 		= ocamlc
CAMLFLAGS 	= -I +sdl -I +site-lib/sdl -g
CAMLLD 		= bigarray.cma sdl.cma sdlloader.cma sdlttf.cma sdlgfx.cma str.cma

SOURCES = utils.ml
SOURCES += image.ml
SOURCES += main.ml

EXEC 	= dcipher

MLIS = $(SOURCES:.ml=.mli)
OBJS = $(SOURCES:.ml=.cmo)

all: $(EXEC)

mlis: $(MLIS)

$(EXEC): $(OBJS)
	$(CAMLC) $(CAMLFLAGS) $(CAMLLD) -o $@ $^

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.mli:
	$(CAMLC) $(CAMLFLAGS) $(CAMLLD) -i $< > $<i

.ml.cmo:
	$(CAMLC) $(CAMLFLAGS) $(CAMLLD) -c $<

clean::
	rm -f *.o *.cm[iox] *~ .*~ #*#

cleanall:: clean
	rm -f $(EXEC) *.mli
