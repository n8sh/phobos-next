#!/bin/bash

DC=ldmd2

# Conservative GC
echo
echo "================"
echo "Conservative GC:"
dub run --compiler=${DC} --build=release-nobounds -- --DRT-gcopt=gc:conservative
echo

# Precise GC with four-way-parallel marking
echo
echo "==========="
echo "Precise GC:"
dub run --compiler=${DC} --build=release-nobounds -- --DRT-gcopt=gc:precise

# Segregated GC
echo
echo "=============="
echo "Segregated GC:"
dub run --compiler=${DC} --build=release-nobounds-segregated-gc
