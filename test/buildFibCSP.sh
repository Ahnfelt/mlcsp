#!/bin/bash
clear
ocamlc -I '../source/' -vmthread threads.cma -c ../source/csp.mli ../source/csp.ml
ocamlc -I '../source/' -c ../source/cspu.ml
ocamlc -I '../source/' -c ../source/legoland.ml
ocamlc -I '../source/' -c fibCSP.ml
ocamlc -I '../source/' -vmthread threads.cma nums.cma -o fibCSP csp.cmo cspu.cmo legoland.cmo fibCSP.cmo