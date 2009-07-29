#!/bin/bash
clear
ocamlc -I '../source/' -vmthread threads.cma -c ../source/csp.mli ../source/csp.ml
ocamlc -I '../source/' -vmthread threads.cma -c ../source/cspu.ml
ocamlc -I '../source/' -vmthread threads.cma -c ../source/regex.ml
ocamlc -I '../source/' -vmthread threads.cma -c proxy.ml
ocamlc -I '../source/' -vmthread unix.cma threads.cma str.cma -o proxy csp.cmo regex.cmo proxy.cmo

