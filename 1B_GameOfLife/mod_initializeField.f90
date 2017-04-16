MODULE mod_initializeField
	IMPLICIT NONE
	
	CONTAINS

	SUBROUTINE createField (spielFeld)
		IMPLICIT NONE
		
		LOGICAL, DIMENSION(30,20) :: spielFeld
		! Schleifenvariablen
		INTEGER :: i,j

		DO i = 1, 30
			DO j = 1, 20
				spielFeld(i,j) = .FALSE.
			END DO
		END DO

	END SUBROUTINE createField
	

	SUBROUTINE createFigures (spielFeld, xPosition, yPosition, pattern)
		IMPLICIT NONE

		LOGICAL, DIMENSION(30,20) :: spielFeld
		! Koordinaten
		INTEGER :: xPosition, yPosition
		! Nummer des Musters
		! Default ist Blinker
! Eigentlich wären Namen schöner, habe ich irgendwie nicht gebacken bekommen.
		INTEGER :: pattern
		
		
		SELECT CASE (pattern)
			!CASE ("blinker")
			CASE (1)
				spielFeld(xPosition, yPosition) = .TRUE.
				spielFeld(xPosition + 1, yPosition) = .TRUE.
				spielFeld(xPosition + 2, yPosition) = .TRUE.
			!CASE ("toad")
			CASE (2)
				spielFeld(xPosition + 1, yPosition + 0) = .TRUE.
				spielFeld(xPosition + 2, yPosition + 0) = .TRUE.	
				spielFeld(xPosition + 3, yPosition + 0) = .TRUE.
				spielFeld(xPosition + 0, yPosition + 1) = .TRUE.
				spielFeld(xPosition + 1, yPosition + 1) = .TRUE.
				spielFeld(xPosition + 2, yPosition + 1) = .TRUE.
			!CASE ("beacon")
			CASE (3)
				spielFeld(xPosition + 0, yPosition + 0) = .TRUE.
				spielFeld(xPosition + 1, yPosition + 0) = .TRUE.
				spielFeld(xPosition + 0, yPosition + 1) = .TRUE.
				spielFeld(xPosition + 3, yPosition + 2) = .TRUE.
				spielFeld(xPosition + 2, yPosition + 3) = .TRUE.
				spielFeld(xPosition + 3, yPosition + 3) = .TRUE.
			!CASE ("glider")
			CASE (4)
				spielFeld(xPosition + 0, yPosition + 0) = .TRUE.
				spielFeld(xPosition + 0, yPosition + 2) = .TRUE.
				spielFeld(xPosition + 2, yPosition + 1) = .TRUE.
				spielFeld(xPosition + 1, yPosition + 2) = .TRUE.
				spielFeld(xPosition + 2, yPosition + 2) = .TRUE.
			! Default "blinker"
			CASE DEFAULT
				spielFeld(xPosition, yPosition) = .TRUE.
				spielFeld(xPosition + 1, yPosition) = .TRUE.
				spielFeld(xPosition + 2, yPosition) = .TRUE.
		END SELECT		

	END SUBROUTINE createFigures

END MODULE mod_initializeField	
