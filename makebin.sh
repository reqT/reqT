#!/bin/bash

if [ -d bin ]; then
    echo "Clear bin"
    rm -rf bin
fi
mkdir -p bin
cd bin
echo "Extracting files in lib:"
for f in ../lib/*
do
    echo $f
    jar xf "$f"
done

cd ..
