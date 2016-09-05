RESULT := rmtld3synth
SOURCES := \
  rmtld3.ml \
  helper.ml \
  rmtld3_synth_test.ml \
  rmtld3synthsmt.ml \
  rmtld3_synthesis.ml
  

#LIBS= unix

ANNOTATE := yes
USE_CAMLP4 := yes

PACKS := unix type_conv sexplib batteries

#PP = camlp4find -echo $(PACKS)
export PP := camlp4find -echo $(PACKS)

all: native-code

OCAMLMAKEFILE := OCamlMakefile
include $(OCAMLMAKEFILE)
