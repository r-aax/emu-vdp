#!/bin/sh

cd ./emu-vdp

echo "rm *.beam"
rm *.beam

for SRC in *.erl
do
    echo "erlc $SRC"
    erlc $SRC
done

cd ..
