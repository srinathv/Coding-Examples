Test: fortest.f95 cudaTest2.o
			gfortran -L /usr/local/cuda/lib -I /usr/local/cuda/include -lcudart -lcuda fortest.f95 cudaTest2.o
cudaTest2.o: cudaTest2.cu 
			nvcc -c -O3 -m64 cudaTest2.cu
clean:
			rm a.out cudaTest2.o cudaTest2.linkinfo
