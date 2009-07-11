#!/bin/bash
clear
ocamlc -I '../source/' -vmthread threads.cma -c ../source/csp.mli ../source/csp.ml
ocamlc -I '../source/' -c ../source/legoland.ml
ocamlc -I '../source/' -vmthread threads.cma -c proxy.ml
ocamlc -I '../source/' -vmthread threads.cma unix.cma str.cma -c proxyCSP.ml
ocamlc -I '../source/' -vmthread threads.cma nums.cma -o proxy csp.cmo legoland.cmo proxy.cmo
ocamlc -I '../source/' -vmthread threads.cma nums.cma unix.cma str.cma -o proxyCSP csp.cmo legoland.cmo proxyCSP.cmo