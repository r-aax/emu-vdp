#!/bin/sh

cd emu-vdp

# Удаление старых файлов документации.
rm -f *.html *.css *.png edoc-info

# Применяем edoc ко всем исходникам.
erl -eval "{ok, L} = file:list_dir(\".\"), edoc:files(lists:filter(fun(F) -> lists:suffix(\".erl\", F) end, L)), io:format(\"doc is built~n\"), halt()"

cd ..
