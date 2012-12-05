
MLS= \
		 oasis2Makefile.ml

CMXS=$(MLS:.ml=.cmx)

CMOS=$(MLS:.ml=.cmo)

MLIS=$(MLS:.ml=.mli)

CMIS=$(MLS:.ml=.cmi)

TRASH=


OASISDIR=../../_build/src/oasis
#OASISDIR=$(shell ocamlfind query oasis)

LIBXS=unix.cmxa $(shell ocamlfind query odn)/odn.cmxa $(OASISDIR)/oasis.cmxa
LIBS=$(LIBXS:.cmxa=.cma)
LIBINC= -I $(OASISDIR)/


BINNAME=oasis2Makefile


OCAMLOPTOPTIONS=-g
OCAMLCOPTIONS=-g
OCAMLOPT=ocamlopt $(OCAMLOPTOPTIONS)
OCAMLC=ocamlc $(OCAMLCOPTIONS)
OCAMLLEX=ocamllex
MENHIROPTIONS=--explain
MENHIR=menhir $(MENHIROPTIONS)
OCAMLDEP=ocamldep


all: $(BINNAME).native

native: $(BINNAME).native

byte: $(BINNAME).byte

$(BINNAME).native: $(CMXS)
	$(OCAMLOPT) $(LIBXS) $(CMXS) -o $(BINNAME).native

$(BINNAME).byte: $(CMOS)
	$(OCAMLC) $(LIBS) $(CMOS) -o $(BINNAME).byte

.PHONY: clean purge test

clean:
	rm -f $(CMOS) $(CMIS) $(CMXS) $(MLS:.ml=.o) $(TRASH)

purge: clean
	rm -f .depend $(BINNAME).native $(BINNAME).byte


depend: $(MLS) $(MLIS)
	$(OCAMLDEP) $(MLS) $(MLIS) >.depend


-include .depend

%.cmo: %.ml
	$(OCAMLC) $(LIBINC) -c $<

%.cmi: %.mli
	$(OCAMLC) $(LIBINC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(LIBINC) -c $<

%.ml %.mli: %.mll
	$(OCAMLLEX) $<

%.ml %.mli: %.mly
	$(MENHIR) $<

test: $(BINNAME).byte
	./$(BINNAME).byte test/_oasis

