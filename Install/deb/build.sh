#!/bin/bash

BUILD_ROOT=/tmp/build-root/c-evo
mkdir -p $BUILD_ROOT
cp -r -f ../.. $BUILD_ROOT
cp -r -f debian $BUILD_ROOT
cd $BUILD_ROOT
dpkg-buildpackage -b -rfakeroot -us -uc
