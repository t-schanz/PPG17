#Compiler
FC="mpif90"
#Number of used Processors
KC=4

main.x: main.f90 mod_calculate.o
	scorep $(FC) -o main.x main.f90 mod_calculate.o

mod_calculate.o: mod_calculate.f90
	$(FC) -c mod_calculate.f90

run: main.x
	mpiexec -n $(KC) ./main.x

clean:
	rm main.x *.mod *.o
