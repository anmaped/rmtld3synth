# Compilation Guide for Windows

## Building `rmtld3synth` on Windows (Cygwin + OCaml 4.14.2)

This guide explains how to compile [`rmtld3synth`](https://github.com/aclysma/rmtld3synth) on **Windows** using **Cygwin** and **OCaml 4.14.2**, with optional support for the **Z3 SMT solver**.


### Prerequisites

- [Cygwin](https://www.cygwin.com/) with development tools installed
- OPAM (installed **inside Cygwin**)
- OCaml 4.14.2


### Basic Installation (Without Z3)

If you **do not need Z3 support**, installing `rmtld3synth` is simple:

```sh
opam install rmtld3synth
````

This installs the base package without the Z3 backend.

### Advanced Installation (With Z3 Support)

To build `rmtld3synth` with Z3 support, you’ll need to manually add a compatible Z3 package to your local OPAM repository.

#### Step-by-Step Instructions

##### 1. Navigate to your local OPAM repository:

```sh
cd ~/.opam/repo/default/packages/z3
```

> Replace the path if your OPAM repo is located elsewhere.

##### 2. Create a directory for Z3 version 4.11.2:

```sh
mkdir -p z3.4.11.2
cd z3.4.11.2
```

##### 3. Download the OPAM file for Z3:

```sh
curl -O https://raw.githubusercontent.com/fdopen/opam-repository-mingw/refs/heads/opam2/packages/z3/z3.4.11.2/opam
```

##### 4. Fix the compiler configuration for Z3 build:

```sh
CC=$(ocamlc -config | awk -F '[\t\r ]+' '/^bytecomp_c_compiler/ {print $2}')
sed -i "s~AR=~CC=$CC AR=~g" opam
```

##### 5. Download the Z3 patch file:

```sh
mkdir -p files
curl -O https://raw.githubusercontent.com/fdopen/opam-repository-mingw/refs/heads/opam2/packages/z3/z3.4.11.2/files/z3-z3-4.11.2.patch
mv z3-z3-4.11.2.patch files/
```

##### 6. Clean and refresh the OPAM repository:

```sh
cd ~/.opam/repo/default
rm ../*.cache 2>/dev/null
opam admin update-extrafiles
```

##### 7. Install Z3 and `rmtld3synth`:

```sh
opam install z3=4.11.2 rmtld3synth
```

---

`rmtld3synth` should now be compiled and available in your current shell. You can confirm the installation by running:

```sh
rmtld3synth --help
```



## To compile rmtld3synth for Windows using ocaml >= 4.04.0 (outdated)

Get [Andreas Hauptmann's installer](https://fdopen.github.io/opam-repository-mingw/installation/) and switch the OCaml compiler to version `>= 4.04.0``.

```shell
opam switch 4.04.0+mingw64
eval `opam config env`
```

In case you have not properly installed the flexdll, download the new flexdll [here](http://alain.frisch.fr/flexdll/flexdll-bin-0.35.zip), and decompress the archive in the current directory (PWD) with the folder name `flexdll-bin-0.35``.

```shell
export PATH=$(PWD)/flexdll-bin-0.35:$PATH
```

Then, you have to pin the rmtld3synth package as described on `compile rmtld3synth for Linux and OS X ` section to conclude the compilation.

:grey_exclamation:HINTS!!

To compile z3 using Cygwin you should use the mingw32 or mingw64 tools, otherwise, it will not work properly.
If the correct version of GCC is not found when executing `mk_make.py`, modify the flags `CXX` and `AR` as needed. For instance:

```shell
CXX=x86_64-w64-mingw32-g++ CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar python scripts/mk_make.py --ml
```

Ensure also that libz3 is properly installed in the current environment.
If the libz3 is not found then use the copy command to copy the `z3/build/libz3.dll.a` library to the `/lib` or `/home/current-user/.opam/compiler-version/lib` directory.