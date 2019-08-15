Bib
===

A low-level OCaml library for parsing BibTeX entries from strings.

Example:

```ocaml
open Core_kernel

let bib = In_channel.with_file "bibliography.bib"
                               ~f:(Fn.compose Bib.entries_from_string
			                      In_channel.input_all)
in (* ... *)
```
