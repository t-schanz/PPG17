mainH.x : main.f90
	f95 -o mainH.x main.f90

runH : mainH.x
	./mainH.x

mainA.x : cc.f90
	f95 -o mainA.x cc.f90

runA : mainA.x
	./mainA.x

run : mainH.x
	./mainH.x

clean :
	rm mainH.x
	rm mainA.x
