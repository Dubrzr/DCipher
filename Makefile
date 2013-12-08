OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep 
OCAMLFLAGS=-I +sdl -I +lablgtk2 -I onnt
OCAMLLD=bigarray.cmxa sdl.cmxa sdlloader.cmxa sdlttf.cmxa lablgtk.cmxa gtkInit.cmx lablgtkspell.cmxa unix.cmxa
OUTPUT=DCipher

# List of files
OCR_OBJS= gui.ml 

all:        $(OCR_OBJS)
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLLD) -o $(OUTPUT) $(OCR_OBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

# Run
run:
	./DCipher

# Clean up
clean:
	rm -f DCipher
	rm -f *.cm[iox]
	rm -f *.o
	rm -f thumbs/temp.bmp thumbs/small.jpg