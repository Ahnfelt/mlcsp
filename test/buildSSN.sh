#!/bin/bash
clear
ocamlc -I '../source/' -vmthread threads.cma -c ../source/csp.mli ../source/csp.ml
ocamlc -I '../source/' -c ../source/legoland.ml
ocamlc -I '../source/' -c ssn.ml
ocamlc -I '../source/' -vmthread threads.cma nums.cma -o ssn csp.cmo legoland.cmo ssn.cmo