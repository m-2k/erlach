#!/bin/bash

command -v ffmpeg   &> /dev/null || { echo "ERR: please install ffmpeg"; exit 1; }
command -v magick   &> /dev/null || { echo "ERR: please install ImageMagick"; exit 1; }
command -v gifsicle &> /dev/null || { echo "ERR: please install gifsicle"; exit 1; }

FPS=7
SCALE=2
LOSSY=40

for MOV_FILE in *.mov; do
  ffmpeg -i "$MOV_FILE" -vf "fps=${FPS},scale=iw/${SCALE}:ih/${SCALE}:flags=lanczos,palettegen" "${MOV_FILE%.mov}-palette.png"
  ffmpeg -i "$MOV_FILE" -i "${MOV_FILE%.mov}-palette.png" -filter_complex "fps=${FPS},scale=iw/${SCALE}:ih/${SCALE}:flags=lanczos[x];[x][1:v]paletteuse" "${MOV_FILE%.mov}-nonopt.gif"
  gifsicle -O3 --lossy="${LOSSY}" "${MOV_FILE%.mov}-nonopt.gif" -o "${MOV_FILE%.mov}.gif"
  rm "${MOV_FILE%.mov}-nonopt.gif"
  rm "${MOV_FILE%.mov}-palette.png"
done

QUALITY=92

for PNG_FILE in *.png; do
  magick "$PNG_FILE" -quality "${QUALITY}" "${PNG_FILE%.png}.avif"
done