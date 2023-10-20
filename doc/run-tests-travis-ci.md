
# run travis-ci build with podman

```shell
podman run -it --rm ubuntu:xenial

apt update && apt install -y \
  git \
  build-essential \
  wget \
  sudo \
  software-properties-common \
  unzip \
  m4 \
  python2.7 \
  libgmp-dev \
  clang-format

adduser user
addgroup user sudo
su user

cd /home/user

git clone https://github.com/anmaped/rmtld3synth.git
cd rmtld3synth
git submodule update --init --recursive

OCAML_VERSION=4.09 TRAVIS_DIST=xenial TRAVIS_OS_NAME=linux BASE_REMOTE=git+https://github.com/ocaml/opam-repository.git bash -ex .travis-opam.sh
```

if you have errors try

```
rm -rf ~/.opam
opam init -a --disable-sandboxing --bare git+https://github.com/ocaml/opam-repository.git
```

or 

```shell
sed -i "s~opam init -a --bare~opam init -a --disable-sandboxing --bare~" .travis-ocaml.sh
rm -rf ~/.opam
```

and retry

```shell
OCAML_VERSION=4.09 TRAVIS_DIST=xenial TRAVIS_OS_NAME=linux BASE_REMOTE=git+https://github.com/ocaml/opam-repository.git bash -ex .travis-opam.sh`
```

## manual (switch)

```shell
opam switch ocaml-base-compiler.4.05.0 
eval $(opam env)

./configure 
make
```
