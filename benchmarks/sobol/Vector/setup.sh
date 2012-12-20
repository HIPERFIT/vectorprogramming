#!/bin/bash

loadHSENV "vanilla"

cabal install --only-dependencies
cabal configure
cabal build

deactivate_hsenv
