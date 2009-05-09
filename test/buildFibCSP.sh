#!/bin/bash
ocamlc -c fib.ml
ocamlc -c fibCSP.ml
ocamlc -o fibCSP fib.cmo fibCSP.cmo