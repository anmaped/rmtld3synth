
all: version
	ocamlbuild -use-ocamlfind rmtld3synth.byte rmtld3synth.native

version:
	echo "let git = \"`git describe --tags` (`git rev-parse HEAD`)\n`uname -m -o` `date +\"%Y-%m-%d %H:%M\"`\"" > version.ml

tests: all
	cd unittests && chmod 777 gen_monitor_tests.sh && ./gen_monitor_tests.sh

clean:
	ocamlbuild -clean
	rm *.byte *.native
