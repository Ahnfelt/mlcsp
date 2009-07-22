#!/bin/bash
clear
ocamlc -I '../source/' -vmthread threads.cma -c ../source/csp.mli ../source/csp.ml
ocamlc -I '../source/' -c ../source/cspu.ml
ocamlc -I '../source/' -c ../source/legoland.ml
ocamlc -I '../source/' -c permission.ml
ocamlc -I '../source/' -vmthread threads.cma nums.cma -o permission csp.cmo cspu.cmo legoland.cmo permission.cmo