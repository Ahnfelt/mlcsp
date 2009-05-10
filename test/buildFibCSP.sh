#!/bin/bash
ocamlc -c fib.ml
ocamlc -c fibCSP.ml
ocamlc -vmthread threads.cma -c ../source/csp.ml
# csp must be compiled before
ocamlc -vmthread threads.cma -o fibCSP fib.cmo fibCSP.cmo ../source/csp.cmo