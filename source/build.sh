#!/bin/bash
ocamlc -vmthread threads.cma -c csp.ml
ocamlc -c legoland.ml
