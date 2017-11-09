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

git clone https://github.com/janestreet/pa_sexp_conv.git pa_sexp_conv
opam pin add pa_sexp_conv pa_sexp_conv/ -n
opam install oasis
opam install pa_sexp_conv

git clone https://github.com/Z3Prover/z3.git z3
cd z3

echo "--- scripts/mk_util.py	2017-08-27 01:00:02.970067000 +0100
+++ scripts/mk_util_new.py	2017-08-27 02:03:47.215391500 +0100
@@ -1930,7 +1930,7 @@
             stubso = os.path.join(self.sub_dir, self.stubs) + '\$(OBJ_EXT)'
             z3dllso = get_component(Z3_DLL_COMPONENT).dll_name + '\$(SO_EXT)'
             out.write('%s: %s %s\n' % (stubso, stubsc, z3dllso))
-            out.write('\t%s -ccopt \"\$(CXXFLAGS_OCAML) -I %s -I %s -I %s \$(CXX_OUT_FLAG)%s\" -c %s\n' %
+            out.write('\t%s -ccopt \"\$(filter-out -std=c++11,\$(CXXFLAGS_OCAML)) -I %s -I %s -I %s \$(CXX_OUT_FLAG)%s\" -c %s\n' %
                       (OCAMLCF, OCAML_LIB, api_src, src_dir, stubso, stubsc))
 
             cmos = ''
" > patch-z3mlflag.patch
patch -d scripts < patch-z3mlflag.patch

python scripts/mk_make.py --ml
cd build
make
sudo PATH=$PATH make install

export OPAMBUILDTEST=0

cd ${OLD_DIR}
# UNTIL HERE !

echo -en "travis_fold:end:prepare.ci\r"
opam config exec -- ci-opam
