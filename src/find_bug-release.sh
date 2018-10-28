#!/bin/bash

for i in $(ls *.d); do
    echo "rdmd $i"
    rdmd-release -main $i
done
