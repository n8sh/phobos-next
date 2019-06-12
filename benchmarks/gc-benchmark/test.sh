#!/bin/bash

# Conservative GC
echo "Conservative GC:"
dub run --build=release-nobounds -- --DRT-gcopt=gc:conservative
echo

# Precise GC with four-way-parallel marking
echo "Precise GC:"
dub run --build=release-nobounds -- --DRT-gcopt=gc:precise
echo

# Segregated GC
echo "Segregated GC:"
dub run --build=release-nobounds-segregated-gc
echo
