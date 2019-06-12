#!/bin/bash

# Conservative GC
dub run --build=release-nobounds -- --DRT-gcopt=gc:conservative

# Precise GC
dub run --build=release-nobounds -- --DRT-gcopt=gc:precisee

# Segregated GC
dub run --build=release-nobounds -- --DRT-gcopt=gc:segregated
