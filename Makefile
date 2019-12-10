MODULES=main draw object actor generator state screen authors
OBJECTS=$(MODULES:=.cmo)
MAIN=main.byte
TEST=test.byte
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=camlimages.png,camlimages.graphics

build:
	$(OCAMLBUILD) -package $(PKGS) $(OBJECTS)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip crossy_caml_src.zip *.ml* _tags Makefile *.md *.png pngs/* *.txt

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)
test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report search_src.zip bisect*.out
