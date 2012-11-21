Branch divergence avoidance
---------------------------

We experiment with the technique for branch divergence avoidance. We
have manually translated the function in `gaussian.hs` to the C-code
in `gaussian.c` which performs array expansion. This didn't succeed
quite well, at least there is still errors.

In `bdiv-example.c` we have taken the gaussian program and simplified
it to ease debugging. There are still the same branches but their
individual effect on the output is a lot clearer.
