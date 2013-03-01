#!/bin/bash

loadHSENV "nikola"

cabal configure
cabal build

deactivate_hsenv