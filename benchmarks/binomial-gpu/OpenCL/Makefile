INCLUDES=-I ./include -I /usr/local/cuda/include
TARGET=build


all:
	mkdir -p build
	g++  -Wpointer-arith   -Wfloat-equal -g3 -ffor-scope $(INCLUDES) -o $(TARGET)/BinomialOption.o -c BinomialOption.cpp
	g++ -o $(TARGET)/BinomialOption $(TARGET)/BinomialOption.o -lpthread -ldl -lSDKUtil -lOpenCL -Llib64
	cp BinomialOption_Kernels.cl build
clean:
	rm -r build
