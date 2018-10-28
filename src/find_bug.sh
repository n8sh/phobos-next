#!/bin/bash

for i in $(ls *.d); do
    echo "rdmd-release -main $i: "
    rdmd-release -main $i

    echo "rdmd-release -main -unittest $i: "
    rdmd-release -main -unittest $i
done
