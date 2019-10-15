#!/bin/bash
if [ "$#" -ne 1 ]; then
  echo "Please provide exactly one file to run"
  exit -1
fi
cp $1 tmpFile
sed -i 's/[a-Z]//g' tmpFile
sed -i 's/</L /g' tmpFile
sed -i 's/>/R /g' tmpFile
sed -i 's/+/A /g' tmpFile
sed -i 's/-/S /g' tmpFile
sed -i 's/\./P /g' tmpFile
sed -i 's/,/G /g' tmpFile
sed -i 's/\[/W /g' tmpFile
sed -i 's/\]/E /g' tmpFile

echo '#include "Brainfuck.h"' > tmpFile.c
echo 'BRAINFUCK_START' >> tmpFile.c
cat tmpFile >> tmpFile.c
rm tmpFile
echo 'BRAINFUCK_END' >> tmpFile.c
gcc -O3 tmpFile.c -o tmpFile
./tmpFile
rm tmpFile
rm tmpFile.c
