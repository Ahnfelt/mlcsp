#!/bin/bash
ocamlc -vmthread threads.cma -c foo.ml
ocamlc -vmthread threads.cma -g -o foo foo.cmo
