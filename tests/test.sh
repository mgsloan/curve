#!/bin/bash

cd ../
./Setup.hs configure --user
./Setup.hs build 
if [[ $? -ne 0 ]]; then
  exit
fi
./Setup.hs install
if [[ $? -ne 0 ]]; then
  exit
fi
cd Test
ghc Main.hs -o Main
if [[ $? -ne 0 ]]; then
  exit
fi
./Main
rm Main

