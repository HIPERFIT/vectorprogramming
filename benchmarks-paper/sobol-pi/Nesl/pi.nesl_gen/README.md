All fcode files share the same pi.cu and pi.so file.

Building the pi.so file on napoleon:

 nvcc -shared --linker-options -soname,pi.so -o pi.so -arch=sm_20 \
      --compiler-options -fPIC --compiler-options -DCUDA -DNDEBUG \
      -I /home/fmma/include/ -I./include -I./vcode pi.cu

Executing one of the examples:

  for i in {1..100}; 
  do 
    /home/dybber/git/build/neslgpu/trunk/nesl/bin/vinterp.cuda -m 51000000 -l pi.nesl_gen/pi.so pi.nesl_gen/pi.10000.fcode;
  done > pi.10000.results
