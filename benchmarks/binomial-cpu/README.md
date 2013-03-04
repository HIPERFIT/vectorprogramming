Benchmark description
---------------------
Binomial pricer implemented in a set of different vector languages. We
price an american put option for different expiration periods.

The R and C implementations were implemented by Rolf Poulsen. Acquired from
http://www.math.ku.dk/~rolf/FAMOES/ and adapted to our benchmarking framework.


Note
----
To get the CUDA version to behave, we currently use a year length of
256 bank days instead of the usual 252 days.


<!-- Old stuff below -->


<!-- Results -->
<!-- ------- -->
<!--  * We've found that nothing beats the hand-coded CUDA version in terms of speed -->
<!--  * Repa (CPU) is second and it seems that Nikola could do well for larger instances -->
<!--  * Nikola is very unreliable (large variance) on large instances (128 years) -->
