MODULES=main draw object actor generator state screen 
OBJECTS=$(MODULES:=.cmo)
MAIN=main.byte
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip crossy_caml_src.zip *.ml* _tags Makefile *.md

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)