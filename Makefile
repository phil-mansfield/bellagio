CC=ocamlc

all: run-tests

Signatures.cmo: Signatures.ml
	$(CC) -c Signatures.ml

Utils.cmi: Signatures.ml Utils.mli
	$(CC) -c Utils.mli

Utils.cmo: Utils.cmi Utils.ml
	$(CC) -c Utils.ml

Grouper.cmi: Signatures.ml Grouper.mli
	$(CC) -c Grouper.mli

Grouper.cmo: Utils.cmi Grouper.cmi Grouper.ml
	$(CC) -c Grouper.ml

Grid.cmi: Signatures.ml Grid.mli
	$(CC) -c Grid.mli

Grid.cmo: Grid.cmi Grid.ml
	$(CC) -c Grid.ml

Histogram.cmi: Signatures.ml Histogram.mli
	$(CC) -c Histogram.mli

Histogram.cmo: Histogram.cmi Histogram.ml
	$(CC) -c Histogram.ml

MCarlo.cmi: Signatures.ml MCarlo.mli
	$(CC) -c MCarlo.mli

MCarlo.cmo: MCarlo.cmi MCarlo.ml
	$(CC) -c MCarlo.ml

Test.cmo: Test.ml Grouper.cmi
	$(CC) -c Test.ml

run-tests: Makefile Signatures.cmo Utils.cmo Grouper.cmo Grid.cmo Histogram.cmo MCarlo.cmo Test.cmo
	$(CC) -o run-tests Signatures.cmo Utils.cmo Grouper.cmo Grid.cmo Histogram.cmo MCarlo.cmo Test.cmo

clean:
	rm run-tests *.cmo *.cmi