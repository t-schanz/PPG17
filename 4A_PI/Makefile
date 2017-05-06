main.x: main.f90 mod_calculate.o
	mpif90 -o main.x main.f90 mod_calculate.o

mod_calculate.o: mod_calculate.f90
	f95 -c mod_calculate.f90


run: main.x
	mpiexec -n 4 ./main.x

clean: 
	rm *.mod *.o main.x
