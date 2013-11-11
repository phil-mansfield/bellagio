OCAMLC := ocamlc.opt
OCAMLOPT := ocamlopt.opt

DB_FLAGS := -g
OPT_FLAGS := -unsafe

MLGSLDIR := ../libs/gsl/ocamlgsl-0.6.0/

ML_FILES := Utils.ml AsciiTable.ml Grouper.ml Grid.ml Histogram.ml MCarlo.ml
MLI_FILES := $(patsubst %.ml,%.mli,$(ML_FILES))
CMO_FILES := $(patsubst %.ml,%.cmo,$(ML_FILES))
CMI_FILES := $(patsubst %.ml,%.cmi,$(ML_FILES))
CMX_FILES := $(patsubst %.ml,%.cmx,$(ML_FILES))
O_FILES := $(patsubst %.ml,%.o,$(ML_FILES))

LIBS := str.cma
LIBS_OPT := str.cmxa

GSL_LIBS := bigarray.cma gsl.cma
GSL_LIBS_OPT := bigarray.cmxa gsl.cmxa

all: renormalize.byte renormalize.native find_critical.byte find_critical.native

Signatures.cmo: Signatures.ml
	$(OCAMLC) $(DB_FLAGS) -c Signatures.ml

Signatures.cmx: Signatures.ml
	$(OCAMLOPT) $(OPT_FLAGS) -c Signatures.ml

%.cmi: %.mli Signatures.ml
	$(OCAMLC) $(DB_FLAGS) -c $<

%.cmo: %.ml %.mli Signatures.ml Utils.mli
	$(OCAMLC) $(DB_FLAGS) -c $<

%.o: %.mli Signatures.ml
	$(OCAMLOPT) $(OPT_FLAGS) -c $<

%.cmx: %.ml %.mli Signatures.ml Utils.mli
	$(OCAMLOPT) $(OPT_FLAGS) -c $<

Renormalize.cmo: Renormalize.ml $(MLI_FILES) Signatures.ml
	$(OCAMLC) $(DB_FLAGS) -c -I $(MLGSLDIR) $<

Renormalize.cmx: Renormalize.ml $(MLI_FILES) Signatures.ml
	$(OCAMLOPT) $(DB_FLAGS) -c -I $(MLGSLDIR) $<

renormalize.byte: Signatures.cmo $(CMI_FILES) $(CMO_FILES) Renormalize.cmo $(MLGSLDIR)/gsl.cma $(MLGSLDIR)/libmlgsl.a
	$(OCAMLC) $(DB_FLAGS) -o $@ Signatures.cmo -I $(MLGSLDIR) -dllpath $(MLGSLDIR) $(LIBS) $(GSL_LIBS) $(CMO_FILES) Renormalize.cmo	

renormalize.native: Signatures.cmx $(O_FILES) $(CMX_FILES) Renormalize.cmx $(MLGSLDIR)/gsl.cmxa $(MLGSLDIR)/libmlgsl.a
	$(OCAMLOPT) $(DB_FLAGS) -o $@ Signatures.cmx -I $(MLGSLDIR) $(LIBS_OPT) $(GSL_LIBS_OPT) $(CMX_FILES) Renormalize.cmx	

Find_Critical.cmo: Find_Critical.ml $(MLI_FILES) Signatures.ml
	$(OCAMLC) $(DB_FLAGS) -c -I $(MLGSLDIR) $<

Find_Critical.cmx: Find_Critical.ml $(MLI_FILES) Signatures.ml
	$(OCAMLOPT) $(DB_FLAGS) -c -I $(MLGSLDIR) $<

find_critical.byte: Signatures.cmo $(CMI_FILES) $(CMO_FILES) Find_Critical.cmo $(MLGSLDIR)/gsl.cma $(MLGSLDIR)/libmlgsl.a
	$(OCAMLC) $(DB_FLAGS) -o $@ Signatures.cmo -I $(MLGSLDIR) -dllpath $(MLGSLDIR) $(LIBS) $(GSL_LIBS) $(CMO_FILES) Find_Critical.cmo	

find_critical.native: Signatures.cmx $(O_FILES) $(CMX_FILES) Find_Critical.cmx $(MLGSLDIR)/gsl.cmxa $(MLGSLDIR)/libmlgsl.a
	$(OCAMLOPT) $(DB_FLAGS) -o $@ Signatures.cmx -I $(MLGSLDIR) $(LIBS_OPT) $(GSL_LIBS_OPT) $(CMX_FILES) Find_Critical.cmx	


clean: 
	rm *.byte *.native *.cm* *.o
