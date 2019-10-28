MODULES=main draw
OBJECTS=$(MODULES:=.cmo)
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)