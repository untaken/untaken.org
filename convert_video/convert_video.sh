#!/bin/bash

# Simply call script with video files/s you want to change and it will 
# execute ffmpeg to encode it to a XVID avi file with mp3 audio:
#
# ./convert_video.sh file.mkv
# 
# More information can be found on:
# http://www.untaken.org/script-to-encode-video-into-a-xvidmp3-format/

for file in "$@"
do

    if [[ ! $file =~ "/" ]]; then
        file="./$file"
    fi
    FILEBASE="${file%.*}"

    COMMAND="mencoder ${file} -o ${FILEBASE}-xvid.avi -oac mp3lame -ovc xvid -xvidencopts bitrate=3000";
    # apparently mencoder is better than ffmpeg for xvids so using line above now
    #COMMAND="ffmpeg -qscale:v 1 -i ${file}  -vcodec mpeg4 -vtag XVID -flags +aic -cmp 2 -subcmp 2 -g 300 -acodec libmp3lame -ar 48000 -ab 128k -ac 2  ${FILEBASE}.avi";

    echo $COMMAND;
    $COMMAND;
done
