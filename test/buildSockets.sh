#!/bin/bash
clear
ocamlc -I '../source/' -vmthread threads.cma -c ../source/csp.mli ../source/csp.ml
ocamlc -I '../source/' -vmthread threads.cma -c sockets.ml
ocamlc -I '../source/' -vmthread unix.cma threads.cma str.cma -o sockets csp.cmo sockets.cmo