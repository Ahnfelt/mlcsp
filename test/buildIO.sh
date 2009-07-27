#!/bin/bash
clear
ocamlc -I '../source/' -vmthread threads.cma -c ../source/csp.mli ../source/csp.ml
ocamlc -I '../source/' -c ../source/cspu.ml
ocamlc -I '../source/' -c ../source/legoland.ml
ocamlc -I '../source/' -vmthread threads.cma -c io.ml
ocamlc -I '../source/' -vmthread threads.cma nums.cma str.cma -o io csp.cmo cspu.cmo legoland.cmo io.cmo
