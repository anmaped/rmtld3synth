RESULT := rmtld3synth
SOURCES := \
  version.ml \
  rmtld3.ml \
  rmtld3_extension.ml \
  rmtld3synth_helper.ml \
  rmtld3synth_smt.ml \
  rmtld3synth_unittest.ml \
  interface/mathkernel.ml \
  interface/texeqparser.ml \
  interface/rmdslparser.ml \
  rmtld3synth_simplify.ml \
  rmtld3synth_cpp11.ml \
  rmtld3synth_ocaml.ml \
  rmtld3synth.ml


ANNOTATE := yes
USE_CAMLP4 := yes

PACKS := unix type_conv sexplib batteries

all: version native-code

version:
	echo "let git = \"`git describe --tags` (`git rev-parse HEAD`)\n`uname -m -o` `date +\"%Y-%m-%d %H:%M\"`\"" > version.ml

tests: version native-code
	cd unittests && chmod 777 gen_monitor_tests.sh && ./gen_monitor_tests.sh

OCAMLMAKEFILE := OCamlMakefile
include $(OCAMLMAKEFILE)
