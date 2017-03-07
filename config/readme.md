
# Configuration files for rmtld3synth
Several configuration files are available in this directory
The next sections describe the steps to automatically construct monitors based on the `usecaseone` and `usecasetwo` files.

## Use Case (1)

Some details of this use case can be found [here](http://rawgit.com/cistergit/rmtld3synth/master/doc/usecase1.html).

Typing `./rmtld3synth.exe -n usecaseone`
in the windows promp we can automatically construct the monitors according to the supplied configuration file.
Ensure that the configuration file is in the same directory of the executable.

The windows binaries are avialble [here](../../releases/download/v0.2-alpha/release-0.2.zip?raw=true).

Linux binaries will be available soon.


## Use Case (2)

Follow the same steps of Use Case (1) but replace the configuration file first.

To be included.


## To compile windows version using ocaml 4.03
```
opam switch 4.03
eval `opam config env`
export PATH=/flexdll-bin-0.35:$PATH
```
Use this [link](http://alain.frisch.fr/flexdll/flexdll-bin-0.35.zip) to download the new flexdll.
Decompress the archive in the root directory with folder name `flexdll-bin-0.35`.
Next, install the packages typing
```
opam install ocamlbuild ocamlfind batteries pa_sexp_conv sexplib type_conv
```
If pa_sexp_conv does not found a valid version we need to compile it manually.
Get version 113.00.02 from [https://github.com/janestreet/pa_sexp_conv](https://github.com/janestreet/pa_sexp_conv) and uncompress it in the folder `pa_sexp_conv`. Use opam to install the compiled version and install them.
```
opam install oasis

git clone https://github.com/janestreet/pa_sexp_conv.git pa_sexp_conv
cd pa_sexp_conv
opam pin add pa_sexp_conv . -n
opam install pa_sexp_conv
```

Compile the rmtld3synth manually using the make command or pin the package and install them.

