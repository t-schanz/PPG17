PROGRAM main
	IMPLICIT NONE
	!	
	! Schleifenvariable
	INTEGER :: i = 0
	! Laufvariable
	INTEGER :: index = 1
	! Arrays und Kardinalitäten
	INTEGER, DIMENSION (1:32) :: base, output
	INTEGER :: baseK = 32, outputK = 0
	! Initialisierung der Arrays
	DO i = 1, 32
		base(i) = i
		output(i) = 0
	END DO

	DO WHILE (baseK > 0)
		! Index anpassen
		index = index + 2
		! Overflow der Laufvariable Index verhindern
		! gegen Ende der Laufzeit sind mehere Durchläufe nötig
		DO i = 1, 3
			IF (index > baseK) THEN
				index = index - baseK
			END IF
		END DO

		! Anhängen an Output-Array
		output (outputK + 1) = base(index)
		outputK = outputK + 1

		! Nummer aus Kreis löschen
		DO i = index, baseK
			base(i) = base(i + 1)
		END DO
		baseK = baseK - 1
	END DO

	!! PRESENTATION DER DATEN
	PRINT *, "Reihenfolge, in der die Personen den Kreis verlassen:"
	PRINT *, output

END PROGRAM main
