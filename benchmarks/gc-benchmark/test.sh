#!/bin/bash

# Conservative GC
dub run --build=release-nobounds -- --DRT-gcopt=gc:conservative
echo

# Precise GC with four-way-parallel marking
dub run --build=release-nobounds -- --DRT-gcopt=gc:precise
echo

# Segregated GC
dub run --build=release-nobounds-segregated-gc
echo
