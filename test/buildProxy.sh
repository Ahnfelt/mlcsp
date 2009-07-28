#!/bin/bash
clear
ocamlc -I '../source/' -vmthread threads.cma -c ../source/csp.mli ../source/csp.ml
ocamlc -I '../source/' -vmthread threads.cma -c ../source/cspu.ml
ocamlc -I '../source/' -vmthread threads.cma -c proxyCSP.ml
ocamlc -I '../source/' -vmthread unix.cma threads.cma str.cma -o proxyCSP csp.cmo cspu.cmo proxyCSP.cmo
