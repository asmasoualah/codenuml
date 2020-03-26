


OCAMLC=ocamlc
OCAMLOPT=ocamlopt 
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
INCLUDES=-I /usr/lib/ocaml/gmp
SRC=./

CHECK-OPT = types.cmx prelude.cmx primitives.cmx print.cmx parser.ml parser.mli parser.cmi parser.cmx lexer.cmx z3parser.ml z3parser.mli z3parser.cmi z3parser.cmx z3lexer.cmx eval.cmx checker.cmx compile.cmx compilegmp.cmx main.cmx

CHECKOPT = gmp.cmxa types.cmx prelude.cmx primitives.cmx print.cmx parser.cmx lexer.cmx z3parser.cmx z3lexer.cmx eval.cmx checker.cmx compile.cmx compilegmp.cmx main.cmx

numl:  $(CHECK-OPT)
	$(OCAMLOPT) $(INCLUDES) -o numl $(CHECKOPT)


# Common rules
.SUFFIXES: .ml .mli .cmx .mll .mly .cmo 

.mll.ml:
	$(OCAMLLEX) $(SRC)$<

.mly.mli:
	$(OCAMLYACC) $(SRC)$<

.mli.ml:
	$(OCAMLC) $(INCLUDES) -c $(SRC)$<

.ml.cmx:
	$(OCAMLOPT) $(INCLUDES) -c $(SRC)$<

.ml.cmo:
	$(OCAMLC) $(INCLUDES) -c $(SRC)$<


# Clean up
clean:
	rm -f $(SRC)*.cmx
	rm -f $(SRC)*.cmi
	rm -f $(SRC)*.cmo
	rm -f $(SRC)*.o
	rm -f $(SRC)lexer.mli lexer.ml z3lexer.mli z3lexer.ml 
	rm -f $(SRC)parser.mli parser.ml z3parser.mli z3parser.ml
	rm -f $(SRC)numl
	rm -f $(SRC)depend
	rm -f $(SRC)comp.ml

depend:
	touch depend ; $(OCAMLDEP) $(INCLUDES) *.mli *.ml > depend

all: depend numl

include depend

