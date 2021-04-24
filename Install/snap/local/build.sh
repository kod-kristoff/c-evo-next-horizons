#!/bin/bash

ln -s Install/snap ../../../snap

pushd ../../..
snapcraft --debug
popd

rm ../../../snap
