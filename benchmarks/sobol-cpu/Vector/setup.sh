#!/bin/bash

loadHSENV "accelerate"

cabal install --only-dependencies
cabal configure
cabal build

deactivate_hsenv
