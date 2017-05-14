main.x: main.f90 
	scorep mpif90 -o main.x main.f90 


run: main.x
	mpiexec -n 4 ./main.x

clean: 
	rm *.mod *.o main.x
