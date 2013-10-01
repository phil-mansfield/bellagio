all: run-tests

Utils.cmi: Utils.mli
	ocamlc -c Utils.mli

Utils.cmo: Utils.ml
	ocamlc -c Utils.ml

GroupGraph.cmi: GroupGraph.mli
	ocamlc -c GroupGraph.mli

GroupGraph.cmo: GroupGraph.ml Utils.mli
	ocamlc -c GroupGraph.ml

Test.cmo: Test.ml GroupGraph.mli
	ocamlc -c Test.ml

run-tests: Utils.cmo GroupGraph.cmo Test.cmo
	ocamlc -o run-tests Utils.cmo GroupGraph.cmo Test.cmo

clean:
	rm run-tests *.cmo *.cmi