MODULE mod_cellNeighbours
	IMPLICIT NONE

	CONTAINS

	! Zählt die Nachbarn einer Zelle in einem Feld, die 
	!  leben, d.h. .TRUE. gesetzt sind.
	!
	SUBROUTINE cellNeighbours (field, x, y, neighbourCount)
		IMPLICIT NONE
		!
		! Anzahl der Nachbarn
		INTEGER :: neighbourCount
		! Spielfeld
		LOGICAL, DIMENSION(30,20) :: field
		! Koordinaten des zu untersuchenden Punktes
		INTEGER :: x, y
		! Feld um Koordinaten mit eventuellen Nachbarn definiert durch:
		INTEGER :: xMin, xMax, yMin, yMax
		! Schleifenvariablen
		INTEGER :: i, j 
		
		! neighbourCount auf Null setzen
		neighbourCount = 0

		!! Initialisierung des umgebenden Feldes 
		!
		! Belegung mit Abstand von 1 zu Koordinate
		xMin = x - 1
		! Anpassung für Abfrage an den Spielfeldrändern
		IF (xMin < 0) THEN
		xMin = xMin + 1
		END IF
		xMax = x + 1
		IF (xMax > 29) THEN
			xMax = xMax - 1
		END IF
		yMin = y - 1
		IF (yMin < 0) THEN
			yMin = yMin + 1
		END IF
		yMax = y + 1
		IF (yMax > 19) THEN
			yMax = yMax - 1
		END IF

		! Eigentliches Zählen, Iteration über Feld 3x3 um Koordinaten
		DO i = xMin, xMax
			DO j = yMin, yMax 
				IF (field(i,j) .eqv. .TRUE.) THEN
					neighbourCount = neighbourCount + 1
				END IF
			END DO
		END DO

		! Korrektur um den Wert an den Koordinaten selbst
		IF (field(x,y) .eqv. .TRUE.) THEN
			neighbourCount = neighbourCount - 1
		END IF

	END SUBROUTINE cellNeighbours

END MODULE mod_cellNeighbours