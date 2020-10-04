#!/bin/sh

cd ./emu-vdp

# Удаление beam, dump, а также файлов документации.
echo "rm -f *.beam erl_crash.dump semantic/*.beam semantic/erl_crash.dump graph/*.beam graph/erl_crash.dump tokens/*.beam tokens/erl_crash.dump *.html *.css *.png edoc-info"
rm -f *.beam erl_crash.dump semantic/*.beam semantic/erl_crash.dump graph/*.beam graph/erl_crash.dump tokens/*.beam tokens/erl_crash.dump *.html *.css *.png edoc-info

# Компилирование всех исходников.
for SRC in *.erl
do
    echo "erlc $SRC"
    erlc $SRC
done

cd ..
