
```
podman run --rm -it --entrypoint='["bash", "-c"]' -v ${PWD}:/workspace/source:z  \
docker.io/anmaped/rmtld3synth \
"cp -r /workspace/source /xx && cd /xx && \
./configure --js_of_ocaml --without-z3-solver && opam exec make js && \
mkdir -p /workspace/source/public && \
cp _build/default/src/rmtld3synth.js /workspace/source/public"
```
