Obtaining sources
=================
Latest version is found through github:

```git clone git://github.com/AccelerateHS/accelerate.git```

Examples are found in the git submodule "accelerate-examples"

```git submodule init accelerate-examples
git submodule update```

The same procedure is followed to get the submodule "accelerate-io"
for reading and writing from different formats (e.g. Repa arrays or
Bitmap image files on desk), which is necessary for certain examples,
or the CUDA bindings from "accelerate-cuda"

```git submodule init accelerate-io
git submodule update
git submodule init accelerate-cuda
git submodule update```


Installation
============
Works on GHC 7.4.1 using cabal, 
