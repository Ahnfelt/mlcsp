#!/bin/bash
ocamlc -vmthread threads.cma -c ../source/csp.ml
ocamlc -I '../source/' -c ../source/legoland.ml
ocamlc -I '../source/' -c ssn.ml
ocamlc -I '../source/' -vmthread threads.cma -o ssn csp.cmo legoland.cmo ssn.cmo