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

# MODIFIED HERE !

OLD_DIR=$(pwd)
cd ..

opam pin add dolmen https://github.com/Gbury/dolmen.git
opam install num

git clone https://github.com/Z3Prover/z3.git z3
cd z3

git checkout 3b1b82bef05a1b5fd69ece79c80a95fb6d72a990

python scripts/mk_make.py --ml
cd build
make
sudo PATH=$PATH make install

export OPAMBUILDTEST=0

cd ${OLD_DIR}

# UNTIL HERE !

echo -en "travis_fold:end:prepare.ci\r"
opam config exec -- ci-opam
