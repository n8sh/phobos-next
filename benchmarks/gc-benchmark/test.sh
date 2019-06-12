#!/bin/bash

# Conservative GC
dub run --build=release-nobounds -- --DRT-gcopt=gc:conservative

# Precise GC with four-way-parallel marking
dub run --build=release-nobounds -- --DRT-gcopt=gc:precise --DRT-gcopt=parallel:4

# Segregated GC
dub run --build=release-nobounds -- --DRT-gcopt=gc:segregated
