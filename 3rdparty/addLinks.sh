#!/bin/bash

SRCDIR=$1
ENDING=$2
DSTDIR=$3

for file in $SRCDIR/*.$ENDING; do ln -s $file $DSTDIR/`basename $file`; done
