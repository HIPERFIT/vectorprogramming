#!/bin/bash

exec ./dist_*/build/binomial-repa/binomial-repa $@ +RTS -N16
