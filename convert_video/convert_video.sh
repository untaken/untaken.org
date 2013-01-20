#!/bin/bash

for file in "$@"
do

    if [[ ! $file =~ "/" ]]; then
        file="./$file"
    fi
    FILEBASE="${file%.*}"

    COMMAND="ffmpeg -qscale:v 1 -i ${file}  -vcodec mpeg4 -vtag XVID -flags +aic -cmp 2 -subcmp 2 -g 300 -acodec libmp3lame -ar 48000 -ab 128k -ac 2  ${FILEBASE}.avi";

    echo $COMMAND;
    $COMMAND;
done
