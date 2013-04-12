#!/bin/bash

loadHSENV "accelerate"

cabal configure
cabal build

deactivate_hsenv