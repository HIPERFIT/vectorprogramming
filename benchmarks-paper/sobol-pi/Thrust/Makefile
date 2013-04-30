
all: pi

sobol.o: sobol.cu
	nvcc -arch=sm_20 -I/usr/local/cuda-5.0/include -o $@ -c $<

computepi.o: computepi.cu
	nvcc -arch=sm_20 -I/usr/local/cuda-5.0/include -o $@ -c $<

main.o: main.cu
	nvcc -arch=sm_20 -I/usr/local/cuda-5.0/include -o $@ -c $<

pi: sobol.o computepi.o main.o
	g++ -m64 -o $@ sobol.o computepi.o main.o -L/usr/local/cuda-5.0/lib64 -lcudart

run: all
	./pi

clean:
	rm -f pi sobol.o computepi.o main.o
