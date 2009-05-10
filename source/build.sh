#!/bin/bash
ocamlc -vmthread threads.cma -c csp.ml
ocamlc -c legoland.ml
ocamlc -vmthread threads.cma -o csp csp.cmo
ocamlc -vmthread threads.cma -o legoland csp.cmo legoland.cmo
