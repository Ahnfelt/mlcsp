#!/bin/bash
clear
ocamlc -I '../source/' -vmthread threads.cma -c ../source/csp.mli ../source/csp.ml
ocamlc -I '../source/' -c ../source/cspu.ml
ocamlc -I '../source/' -c ../source/legoland.ml
ocamlc -I '../source/' -c commstime.ml
ocamlc -I '../source/' -vmthread unix.cma threads.cma nums.cma -o commstime csp.cmo cspu.cmo legoland.cmo commstime.cmo