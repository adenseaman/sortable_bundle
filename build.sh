#!/bin/bash

DIR=$(pwd)

cd $DIR/distributed_map
rm ebin/*.beam
erl -make
cd $DIR/sortable_challenge
rm ebin/*.beam
erl -make
