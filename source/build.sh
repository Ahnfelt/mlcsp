#!/bin/bash
ocamlc -vmthread threads.cma -c csp.mli csp.ml
ocamlc -c cspu.ml
ocamlc -c legoland.ml
ocamldoc -html -d ../docs/ csp.mli