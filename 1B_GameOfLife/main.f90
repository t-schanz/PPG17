

SUBROUTINE print2D (field)
	IMPLICIT NONE
	!
	LOGICAL, DIMENSION(30, 20) :: field
	! Schleifenvariablen
	INTEGER :: i, j
	!
	CHARACTER :: life, dead
	life = "#"
	dead = "."

	DO i = 1, 20
		DO j = 1, 30
			IF (field(j ,i) .eqv. .TRUE.) THEN
				write(*,"(A)",advance="no") life
			ELSE
				write(*,"(A)",advance="no") dead
			END IF
		END DO
		! new line
		write(*,"(A)") ""
	END DO


END SUBROUTINE print2D

!! Game Of Life
!createField
!createFigures
!developLife
!printTwoDLogical
	

PROGRAM main
	
	USE mod_lifeCycle

	IMPLICIT NONE
	!
	! Spielfeld
	LOGICAL, DIMENSION(30, 20) :: spielFeld
	! Scalarfeld mich werten aus countNeighbours
	INTEGER, DIMENSION(30,20) :: neighbourField
	! Schleifenvariablen, Hilfsvariablen
	INTEGER :: i = 0, j = 0, n = 0
	! Position der Figur
	INTEGER :: xPosition = 0, yPosition = 0
	!
	!
	CHARACTER, DIMENSION(30,20) :: printField


	!createField
	DO i = 1, 30
		DO j = 1, 20
			spielFeld(i,j) = .FALSE.
		END DO
	END DO


	!createFigures
	xPosition = 3
	yPosition = 3
	! Blinker
	spielFeld(xPosition, yPosition) = .TRUE.
	spielFeld(xPosition + 1, yPosition) = .TRUE.
	spielFeld(xPosition + 2, yPosition) = .TRUE.
	PRINT *, spielFeld



	!developLife

	CALL developLife(spielFeld, neighbourField)
	
	CALL print2D(spielFeld)
	CALL developLife(spielFeld, neighbourField)
	



	!printTwoDLogical
	CALL print2D(spielFeld)
	




	



END PROGRAM main