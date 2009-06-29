#!/bin/bash
clear
#ocamlc -thread unix.cma threads.cma -c foo.ml
#ocamlc -thread unix.cma threads.cma -g -o foo foo.cmo
ocamlc -vmthread threads.cma -c simple.ml
ocamlc -vmthread threads.cma -g -o simple simple.cmo
