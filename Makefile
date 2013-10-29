all: bytecode-tests native-code-tests

Signatures.cmo: Signatures.ml
	ocamlc.opt -g -c Signatures.ml

Signatures.o: Signatures.ml
	ocamlopt -c -c Signatures.ml

Utils.cmi: Signatures.ml Utils.mli
	ocamlc.opt -g -c Utils.mli

Grouper.cmi: Signatures.ml Grouper.mli
	ocamlc.opt -g -c Grouper.mli

Grid.cmi: Signatures.ml Grid.mli
	ocamlc.opt -g -c Grid.mli

Histogram.cmi: Signatures.ml Histogram.mli
	ocamlc.opt -g -c Histogram.mli

MCarlo.cmi: Signatures.ml MCarlo.mli
	ocamlc.opt -g -c MCarlo.mli

Utils.cmo: Utils.cmi Utils.ml
	ocamlc.opt -g -c Utils.ml

Grouper.cmo: Utils.cmi Grouper.cmi Grouper.ml
	ocamlc.opt -g -c Grouper.ml

Grid.cmo: Grid.cmi Grid.ml
	ocamlc.opt -g -c Grid.ml

Histogram.cmo: Histogram.cmi Histogram.ml
	ocamlc.opt -g -c Histogram.ml

MCarlo.cmo: MCarlo.cmi MCarlo.ml
	ocamlc.opt -g -c MCarlo.ml

Test.cmo: Test.ml Grouper.cmi
	ocamlc.opt -g -c Test.ml

Utils.o: Utils.cmi Utils.ml
	ocamlopt -g -c Utils.ml

Grouper.o: Utils.cmi Grouper.cmi Grouper.ml
	ocamlopt -g -c Grouper.ml

Grid.o: Grid.cmi Grid.ml
	ocamlopt -g -c Grid.ml

Histogram.o: Histogram.cmi Histogram.ml
	ocamlopt -g -c Histogram.ml

MCarlo.o: MCarlo.cmi MCarlo.ml
	ocamlopt -g -c MCarlo.ml

Test.o: Test.ml Grouper.cmi
	ocamlopt -g -c Test.ml

bytecode-tests: Makefile Signatures.cmo Utils.cmo Grouper.cmo Grid.cmo Histogram.cmo MCarlo.cmo Test.cmo
	ocamlc.opt -g -o bytecode-tests Signatures.cmo Utils.cmo Grouper.cmo Grid.cmo Histogram.cmo MCarlo.cmo Test.cmo

native-code-tests: Makefile Signatures.o Utils.o Grouper.o Grid.o Histogram.o MCarlo.o Test.o
	ocamlopt -g -o native-code-tests Signatures.o Utils.o Grouper.o Grid.o Histogram.o MCarlo.o Test.o

clean:
	rm run-tests *.cmo *.cmi *.o *.cmx