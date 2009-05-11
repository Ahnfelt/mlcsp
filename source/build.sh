#!/bin/bash
ocamlc -vmthread threads.cma -c csp.ml
ocamlc -c legoland.ml
ocamlc -c test.ml
ocamlc -vmthread threads.cma -o legoland csp.cmo legoland.cmo
ocamlc -vmthread threads.cma -o test csp.cmo test.cmo

