#!/bin/bash

loadHSENV "vanilla"

cabal configure
cabal build

deactivate_hsenv