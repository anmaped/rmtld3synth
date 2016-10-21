RESULT := rmtld3synth
SOURCES := \
  rmtld3.ml \
  rmtld3synth_helper.ml \
  rmtld3synth_unittest.ml \
  rmtld3synth_smt.ml \
  rmtld3synth.ml
  

#LIBS= unix

ANNOTATE := yes
USE_CAMLP4 := yes

PACKS := unix type_conv sexplib batteries

#PP = camlp4find -echo $(PACKS)
export PP := camlp4find -echo $(PACKS)

all: native-code

OCAMLMAKEFILE := OCamlMakefile
include $(OCAMLMAKEFILE)
