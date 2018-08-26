# HTTP/2 framing

Exploring HTTP/2 framing in OCaml. Attempting to use angstrom/faraday in the hope that it becomes easy to port this to be used within [httpaf](https://github.com/inhabitedtype/httpaf).

### Goal

Aim to provide decoder/encoder and HTTP/2 types to be consumed by other projects.

### Contributing

* Look for the unfinished payload types from `./lib/types.mli`
* Parsing is done in `./lib/parse.ml`
