MODULE mod_lifecycle

	! Subroutine zum Zählen der Nachbarn einer Zelle ausgelagert
	USE mod_cellNeighbours
	
	IMPLICIT NONE

	CONTAINS

	! Erstellt ein Skalarfeld mit den Anzahlen der Nachbarn
	!  jeder Zelle des Spielfeld
	!
	SUBROUTINE countNeighbours (field, neighbourField)
		IMPLICIT NONE
		!
		LOGICAL, DIMENSION(30, 20) :: field
		INTEGER, DIMENSION(30, 20) :: neighbourField
		! Schleifenvariablen
		INTEGER :: i, j
		! Hilfsvariable
		INTEGER :: n

		DO i = 1, 30
			DO j = 1, 20
				n = 0
				CALL neighbours(field, i, j, n)
				neighbourField(i,j) = n
			END DO
		END DO

	END SUBROUTINE countNeighbours

	! Wendet die Regeln von Convays Game of Life 
	!  auf das Spielfeld an.
	SUBROUTINE developLife (field, neighbourField)
		IMPLICIT NONE
		!
		LOGICAL, DIMENSION(30, 20) :: field
		INTEGER, DIMENSION(30, 20) :: neighbourField
		! Schleifenvariablen
		INTEGER :: i, j

		! Erstellen des Skalarfelds mit Nachbarschaftszahlen
		CALL countNeighbours(field, neighbourField)

		! Anwenden der Regeln für die neue Runde
		DO i = 1, 30
			DO j = 1, 20
				IF (field(i, j) .eqv. .TRUE.) THEN
					field(i,j) = ((neighbourField(i,j) > 1) .AND. (neighbourField(i,j) < 4))
				ELSE
					field(i,j) = (neighbourField(i,j) == 3)
				END IF
			END DO
		END DO

	END SUBROUTINE

END MODULE mod_lifecycle