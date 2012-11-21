Longstaff & Schwartz implementations
------------------------------------

The Python implementation is taken from the presentation here:
https://ep2012.europython.eu/conference/talks/derivatives-analytics-with-python-numpy

The Haskell Vector implementation does not currently compute the same
result as the Python implementation. We don't understand why, but
seems to be a problem with the method we use to solve the linear least
squares problem which is taken from page 109-110 in the "Scientific
Computing: An introductory survey" by Michael T. Heath.


Results
-------
 * Access to the functionality of LAPACK (CUBLAS/CULA) would be a nice
   in any vector language for an application such as this.

 * Or alternatively: optimizing smaller applications such as matrix
   multiplication, Cholesky factorization, forward/backward
   substitution etc. would be beneficial for the performance.

