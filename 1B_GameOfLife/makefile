# Compiler
#FC = "f95 -fimplicit-none " (das habe ich irgendwo gelesen, soll einem das ewige IMPLICIT NONE ersparen. 
#				Hat bei mir aber nicht funktionert. Könnt ihr ja mal ausprobieren.)
FC = "f95"

main.x: main.o mod_lifeCycle.o mod_initializeField.o
	$(FC) -o main.x main.o mod_lifeCycle.o mod_initializeField.o
 
main.o: main.f90 
	$(FC) -c main.f90 

mod_lifeCycle.o: mod_lifeCycle.f90 
	$(FC) -c mod_lifeCycle.f90 

mod_initializeField.o: mod_initializeField.f90
	$(FC) -c mod_initializeField.f90

run: main.x
	./main.x

clean: 
	rm *.mod *.o main.x

