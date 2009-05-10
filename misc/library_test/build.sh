#!/bin/bash
ocamlc -c module1.ml
ocamlc -c module2.ml
ocamlc -c libs/module3.ml
ocamlc -I 'libs/' -c main.ml
ocamlc -I 'libs/' -o main module1.cmo module2.cmo module3.cmo main.cmo