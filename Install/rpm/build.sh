#!/bin/bash

tar -zcvf c-evo.tar.gz -c ../.. .
cp c-evo.tar.gz ~/rpmbuild/SOURCES
rpmbuild -v -ba c-evo.spec
