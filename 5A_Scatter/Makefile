main.x: main.f90 mod_initialize.o
	mpif90 -o main.x main.f90 mod_initialize.o

mod_initialize.o: mod_initialize.f90
	f95 -c mod_initialize.f90


run: main.x
	mpiexec -n 4 ./main.x

clean: 
	rm *.mod *.o main.x
