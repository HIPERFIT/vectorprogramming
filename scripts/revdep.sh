#!/bin/zsh

# remember to compile cabaldepends.hs and sortversions.hs!

# for a tip to efficient recursive search:
# (in zsh)
# ----------------------------------
# cat file.revdeps|while read p; do                                                                                                             !25715
# revdep.sh ${=p}
# done
# ----------------------------------

searchPkg="$1"
searchVer="$2"

if [ -d hackage ]; then
  echo found hackage files
else
  echo extracting hackage index
  mkdir hackage
  tar -xmf $HOME/.cabal/packages/hackage.haskell.org/00-index.tar -C hackage
  rm -r hackage/preferred-versions
fi

echo "checking revdeps for ${searchPkg} version $searchVer"

touch ${searchPkg}.revdeps

pTot=`ls hackage| wc -l`
echo found $pTot packages

pNum=0
cd hackage

for p in *
do
  (
  latest=`ls --color=never $p|xargs basename -a| ../sortversions | tail -n 1`

  # hack: asterisk after $latest to deal with version tags :-(
  for cabalfile in $p/$latest*/*.cabal
  do
    grep "\<${searchPkg}\>" $cabalfile > /dev/null
    if [ $? = 0 ]; then
      # eliminate false positives via Cabal.
      ../cabaldepends $cabalfile ${searchPkg} ${searchVer}
      if [ $? = 0 ]; then
        #echo "\n$p"
        echo "$p $latest " >> ../${searchPkg}.revdeps
        #else
        #  echo "\nfalse positive $p (${cabalfile}"
      fi
    fi
  done) || true

  pNum=$((1+pNum))
  printf "checked %.2f %%\r" $(( $pNum.0 / $pTot.0 * 100.0))

done

echo "\n"
