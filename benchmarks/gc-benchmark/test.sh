#!/bin/bash

# Conservative GC
dub run --build=release-nobounds -- --DRT-gcopt=gc:conservative

# Precise GC
dub run --build=release-nobounds -- --DRT-gcopt=gc:precise --DRT-gcopt=parallel:4

# Segregated GC
dub run --build=release-nobounds -- --DRT-gcopt=gc:segregated
