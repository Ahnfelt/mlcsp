#!/bin/bash
clear
ocamlc -I '../source/' -vmthread threads.cma -c ../source/csp.mli ../source/csp.ml
ocamlc -I '../source/' -c even.ml
ocamlc -I '../source/' -vmthread threads.cma -o even csp.cmo even.cmo