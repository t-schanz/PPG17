# Compiler
#FC = "f95 -fimplicit-none "
FC = "f95"

main.x: main.f90 mod_lifeCycle.o
	$(FC) -o main.x main.f90 mod_lifeCycle.o

mod_lifeCycle.o: mod_lifeCycle.f90 mod_cellNeighbours.o
	$(FC) -c mod_lifeCycle.f90 mod_cellNeighbours.o

mod_cellNeighbours.o: mod_cellNeighbours.f90
	$(FC) -c mod_cellNeighbours.f90	

run:main.x
	./main.x

clean: 
	rm *.mod *.o main.x
