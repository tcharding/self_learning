#!/bin/bash
#
# new-file.sh: create new file, add Makfile entry and .gitignore exe

if [ $# -eq 0 ]; then
    echo "Usage: new-file.sh name" >&2
    exit 0
fi

name=$1
cp template.c $1.c
echo $1 >> .gitignore

# add a Makefile entry
