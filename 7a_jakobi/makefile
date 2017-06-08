# Common definitions
CC = f95
MPIC = mpif90

# Number of Processors
NP = 8

# Compiler flags:
FLAGS =  -Wextra -O3
# flags for debugging:
#FLAGS =  -Wextra -Wall -g

main.x: main2.f90 mod_initialize.o mod_finalize.o mod_jakobi.o mod_communicate.o 
	$(MPIC) -o main.x main2.f90 mod_initialize.o mod_finalize.o mod_jakobi.o mod_communicate.o

#main.x: main.f90 mod_initialize.o mod_calc.
#	$(MPIC) -o main.x main.f90 mod_initialize.o mod_calc.o

# finalize hier f debugging print!
mod_initialize.o: mod_initialize.f90 mod_finalize.o
	$(CC) -c mod_initialize.f90 mod_finalize.o

mod_finalize.o: mod_finalize.f90
	$(CC) -c mod_finalize.f90

mod_jakobi.o: mod_jakobi.f90
	$(CC) -c mod_jakobi.f90

mod_communicate.o: mod_communicate.f90
	$(MPIC) -c mod_communicate.f90

run: main.x
	mpiexec -n $(NP) ./main.x

clean:
	rm *.mod *.o main.x

