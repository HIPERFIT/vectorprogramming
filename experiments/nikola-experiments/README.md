In this folder you will find two experiments using Nikola


binomial
--------
In this experiment we have attempted to move a larger portion of the
binomial pricer in Nikola from the CPU to the GPU. Namely the
generation of the vectors uPow and dPow. This is found in the binomial
subdirectory, more precisely in "binom" function of
binomial/American/Nikola/Implementation.hs

This fails for several reasons:

 * generate does not compile
 * some bug makes the Nikola compiler run forever using lots and lots
   of memory. After 10 minutes around 60% of napoleons memory was used.
   
   we should simplify this and send it to Geoffrey


small_tests
-----------
Smaller experiments using Nikola has been carried out in
"small_tests/Main.hs". Findings:

 * Nikola does not seem to be able to compile its own fromFunction and
   generate functions.
 
 * You have to be very careful around enumerating ranges. This will
   not compile:
   
     example1 :: Exp Int32 -> Exp Int32
     example1 n = foldl (+) 0 [1..n]
   
   because we don't know the concrete value of `n` yet.
