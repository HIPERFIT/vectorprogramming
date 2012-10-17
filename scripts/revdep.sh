#!/bin/zsh

searchPkg="$1"

if [ -d hackage ]; then
  echo found hackage files
else
  echo extracting hackage index
  mkdir hackage
  tar -xmf $HOME/.cabal/packages/hackage.haskell.org/00-index.tar -C hackage
fi

echo "checking revdeps for ${searchPkg}"

touch ${searchPkg}.revdeps

pTot=`ls hackage| wc -l`
echo found $pTot packages

pNum=0
cd hackage
for p in *
do

  (for cabalfile in $p/*/*.cabal
  do
    grep "\<${searchPkg}\>" $cabalfile > /dev/null
    if [ $? = 0 ]; then
      echo \n$p\n
      echo $cabalfile >> ../${searchPkg}.revdeps
    fi
  done) || true

  pNum=$((1+pNum))
  printf "checked %.2f %%\r" $(( $pNum.0 / $pTot.0 * 100.0))

done
