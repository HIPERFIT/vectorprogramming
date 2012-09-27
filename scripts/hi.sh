#!/bin/bash

# Make sure you have executed the following commands before running
# this script:
#   cabal update
#   cabal install c2hs alex happy

# These path variables should be set in your .profile:
#   PATH=~/.cabal/bin/:/usr/local/cuda/bin/:$PATH
#   LD_LIBRARY_PATH=/usr/local/cuda/lib64:$LD_LIBRARY_PATH
#   LD_LIBRARY_PATH=/usr/local/cuda/lib:$LD_LIBRARY_PATH

ROOT=`pwd`
cd $ROOT

BASEPATH=$PATH
BASEGHC=`ghc --version | awk -F' ' '{print $NF}'`

which deactivate_hsenv
if [ $? -eq 0 ]; then
    deactivate_hsenv
fi

fetchGHC() {
  mkdir -p $GHC_DIR
  cd $GHC_DIR
  if [ ! -f ghc-$GHC_VERSION-x86_64-unknown-linux.tar.bz2 ]; then
      wget http://www.haskell.org/ghc/dist/$GHC_VERSION/ghc-$GHC_VERSION-x86_64-unknown-linux.tar.bz2
  fi
}

# Creates a hsenv directory for the selected project, with the
# selected GHC version.  Enters the directory and activates hsenv.
init_hsenv() {
  NAME=$1
  GHC_VERSION=$2
  GHC_DIR=$ROOT/ghc/ghc-$GHC_VERSION
  INSTALL_DIR=$ROOT/$NAME-GHC$GHC_VERSION/

  if [ -d $INSTALL_DIR ]; then
    echo "$INSTALL_DIR already exists, using old installation"
    cd $INSTALL_DIR
  else
      fetchGHC
      mkdir -p $INSTALL_DIR
      cd $INSTALL_DIR
      hsenv --ghc=$GHC_DIR/ghc-$GHC_VERSION-x86_64-unknown-linux.tar.bz2
  fi

  source .hsenv_$NAME-GHC$GHC_VERSION/bin/activate
  cabal install cabal-install
}

installCUDAFromHackage (){
  # Fix and install cuda bindings
  cabal unpack cuda
  cd cuda-*
  sed -i -e "s/import Foreign.CUDA.Driver.Exec$/import Foreign.CUDA.Driver.Exec hiding \(Max\)/" Foreign/CUDA/Driver.hs 
  autoconf
  cabal install --extra-include-dirs=/usr/local/cuda/include/
  cd ..
}

installAccelerateFromHackage(){
  init_hsenv "accelerate-hackage" "7.4.1"

    cabal install accelerate accelerate-io

    installCUDAFromHackage

    cabal install accelerate-cuda --extra-include-dirs=/usr/local/cuda/include/
    cabal install accelerate-examples
  deactivate_hsenv
}

# This doesn't work currently
installAccelerateFromGithub() {
  init_hsenv "accelerate-git" "7.4.1"

    PATH=~/.cabal/bin/:/usr/local/cuda/bin/:$BASEPATH
    LD_LIBRARY_PATH=/usr/local/cuda/lib64

    
    git clone git://github.com/tmcdonell/cuda.git
    cd cuda
    autoconf
    cabal install --extra-include-dirs=/usr/local/cuda/include/
    cd ..

    git clone git://github.com/AccelerateHS/accelerate.git
    cd accelerate
    git submodule init accelerate-io
    git submodule init accelerate-cuda
    git submodule update

    cabal install
    cd accelerate-io
    cabal install
    cd ../accelerate-cuda/
    autoconf
    cabal install --extra-include-dirs=/usr/local/cuda/include/ \
                  --extra-lib-dirs=/usr/local/cuda/lib64/
    cd ../..
  deactivate_hsenv
}

installNikola() {
  init_hsenv "nikola" "7.4.2"

    installCUDAFromHackage
    cabal install test-framework-quickcheck2 test-framework-hunit

    git clone git://github.com/mainland/nikola.git
    cd nikola

    cabal configure \
        --disable-library-profiling \
        --enable-tests \
        --flags=examples \
        --configure-option="--with-nvcc=/usr/local/cuda/bin/nvcc"

    cabal build
    cabal install

  deactivate_hsenv
}

installFeldspar() {
  init_hsenv "feldspar" "7.0.2"
    cabal install feldspar-language 

    cabal unpack feldspar-compiler
    cd feldspar-compiler*
    sed -i -e "s/ExplicitForall/ExplicitForAll/" Feldspar/NameExtractor.hs
    cabal install
    cd ..
  deactivate_hsenv
}

installObsidian() {
  init_hsenv "obsidian" "7.0.3"
    git clone git://github.com/svenssonjoel/GCDObsidian.git obsidian
    cd obsidian
    cabal install

  deactivate_hsenv
}

#installObsidian
#installFeldspar
#installAccelerateFromHackage
installNikola
#installAccelerateFromGithub
