#!/usr/bin/env bash
input=$(printf "(yl\n$(cat $1)\n)")
# echo "$input"
emacs -Q --batch -l yololisp-compiler.el --eval "$input"
echo
