{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String
import Development.Shake
import Development.Shake.FilePath
import Shelly hiding ((<.>))
import Text.Regex

main = shake shakeOptions $ do
  want ["master.pdf"]

  "master.pdf" *> \out -> do
    let baseFile = dropExtension out
    -- need [baseFile <.> "tex"]
    need [
     "defaultprelude.tex",
     "finance.tex",
     "intro.tex",
     "master.tex",
     "ourwork.tex",
     "../bibliography/bibliography.bib",
     "survey.tex"]
    pdflatex $ baseFile <.> "tex"
    bibtex   $ baseFile
    pdflatex $ baseFile <.> "tex"
    pdflatex $ baseFile <.> "tex"

pdflatex file = shelly $ run_ "pdflatex" [ fromString file]
bibtex file = shelly $ run_ "bibtex" [fromString file]
