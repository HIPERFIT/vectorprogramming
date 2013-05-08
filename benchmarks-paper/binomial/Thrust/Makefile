all: binomial

binomial.o: binomial.cu
	nvcc -arch=sm_20 -I/usr/local/cuda-5.0/include -o $@ -c $<

binomial: binomial.o
	g++ -m64 -o $@ binomial.o -L/usr/local/cuda-5.0/lib64 -lcudart

run: all
	./binomial

clean:
	rm -f binomial binomial.o
