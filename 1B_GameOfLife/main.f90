PROGRAM main
		
	USE mod_lifeCycle
	USE mod_print
	USE mod_initializeField

	IMPLICIT NONE
	!
	! Spielfeld
	LOGICAL, DIMENSION(30, 20) :: spielFeld
	! Position der Figur
	INTEGER :: xPosition = 3 
	INTEGER :: yPosition = 3
	! gewünschtes Muster
	! 1 = blinker
	! 2 = toad
	! 3 = beacon
	! 4 = glider
	INTEGER :: pattern = 3
	! Schleifenvariable
	INTEGER :: i


	CALL createField(spielFeld)
	CALL createFigures(spielFeld, xPosition, yPosition, pattern)
	
	DO i = 0, 999
		CALL developLife(spielFeld)
		CALL print2D(spielFeld)
	END DO	

END PROGRAM main
