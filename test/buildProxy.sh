#!/bin/bash
clear
ocamlc -I '../source/' -vmthread threads.cma -c ../source/csp.mli ../source/csp.ml
ocamlc -I '../source/' -c ../source/legoland.ml
ocamlc -I '../source/' -vmthread threads.cma -c proxy.ml
ocamlc -I '../source/' -vmthread threads.cma nums.cma -o proxy csp.cmo legoland.cmo proxy.cmo