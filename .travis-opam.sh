echo -en "travis_fold:start:prepare.ci\r"
# If a fork of these scripts is specified, use that GitHub user instead
fork_user=${FORK_USER:-ocaml}

# If a branch of these scripts is specified, use that branch instead of 'master'
fork_branch=${FORK_BRANCH:-master}

### Bootstrap

set -uex

get() {
  wget https://raw.githubusercontent.com/${fork_user}/ocaml-ci-scripts/${fork_branch}/$@
}

get .travis-ocaml.sh
sh .travis-ocaml.sh

export OPAMYES=1
eval $(opam config env)

opam depext -y conf-m4
opam pin add travis-opam https://github.com/${fork_user}/ocaml-ci-scripts.git#${fork_branch}

# include other packages
opam pin add dolmen https://github.com/Gbury/dolmen.git#e81b130ac0fdcd7e2b08603648c54c8ead8fbd7b
opam pin add dolmen-export https://github.com/Gbury/dolmen.git#e81b130ac0fdcd7e2b08603648c54c8ead8fbd7b
opam install base
opam install z3=4.8.4 -v
[ -f "`ocamlfind query z3`/libz3.so" ] && sudo cp `ocamlfind query z3`/libz3.so /usr/lib
#[ -f "`ocamlfind query z3`/libz3.dylib" ] && sudo cp `ocamlfind query z3`/libz3.dylib ~/Library/


echo -en "travis_fold:end:prepare.ci\r"
opam config exec -- ci-opam
