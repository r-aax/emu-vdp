#!/bin/sh

cd emu-vdp

# Точка входа main:test().
erl -eval "main:test(), halt()"

cd ..
