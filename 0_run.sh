#!/bin/sh

cd emu-vdp

# Точка входа main:start.
erl -eval "main:start(), halt()"

cd ..
