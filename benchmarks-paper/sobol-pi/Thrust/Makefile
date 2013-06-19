all: computepi

computepi.o: computepi.cu
	nvcc -arch=sm_20 -I/usr/local/cuda-5.0/include -o $@ -c $<

main.o: main.cu
	nvcc -arch=sm_20 -I/usr/local/cuda-5.0/include -o $@ -c $<

computepi: computepi.o main.o
	g++ -m64 -o $@ computepi.o main.o -L/usr/local/cuda-5.0/lib64 -lcudart

run: all
	./computepi

clean:
	rm -f computepi computepi.o main.o
