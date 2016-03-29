#!/bin/bash
#
# new-file.sh: create new file, add Makfile entry and .gitignore exe

if [ $# -eq 0 ]; then
    echo "Usage: new-file.sh name" >&2
    exit 0
fi

name=$1
ls $name.c 2> /dev/null
if [ $? -eq 0 ]; then
    echo "File already exists $name.c"
    exit 1
fi

cp template.c $1.c
#echo $1 >> .gitignore

exit 0

# add a Makefile entry
