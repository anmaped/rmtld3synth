RESULT := rmtld3synth
SOURCES := \
  rmtld3.ml \
  rmtld3synth_helper.ml \
  rmtld3synth_smt.ml \
  rmtld3synth_unittest.ml \
  interface/mathkernel.ml \
  rmtld3_extension.ml \
  rmtld3synth_simplify.ml \
  rmtld3synth.ml \


ANNOTATE := yes
USE_CAMLP4 := yes

PACKS := unix type_conv sexplib batteries

all: native-code

OCAMLMAKEFILE := OCamlMakefile
include $(OCAMLMAKEFILE)
