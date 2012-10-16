#!/bin/zsh

searchPkg="$1"

if [ -d hackage ]; then
  echo found hackage files
else
  echo extracting hackage index
  mkdir hackage
  tar -xf $HOME/.cabal/packages/hackage.haskell.org/00-index.tar -C hackage
fi

echo "checking revdeps for ${searchPkg}"

touch ${searchPkg}.revdeps

echo found `ls hackage| wc -l` packages

pNum=0
cd hackage
for p in *
do

  (for cabalfile in $p/*/*.cabal
  do
    grep "\<${searchPkg}\>" $cabalfile > /dev/null
    if [ $? = 0 ]; then
      echo $p
      echo $cabalfile >> ../${searchPkg}.revdeps
    fi
  done) || true

  pNum=$((1+pNum))
  echo checked ${pNum} packages

done
