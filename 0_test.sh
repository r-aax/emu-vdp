#!/bin/sh

cd emu-vdp

if [[ -z "$1" ]]
then
    # Точка входа main:test().
    erl -eval "main:test(), halt()"
    
else
    # Точка входа - в реализации конкретного эмулятора. "$1":test().
    erl -eval $1":test()"
fi

cd ..
