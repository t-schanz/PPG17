! NOT IN USE !

MODULE mod_print 
	IMPLICIT NONE
	
	CONTAINS
	
	! Druckt das Spielfeld
	SUBROUTINE print2D (field)
		IMPLICIT NONE
		!	
		LOGICAL, DIMENSION(30, 20) :: field
		! Schleifenvariablen
		INTEGER :: i, j
		! ASCI-Zeichen für lebende und tote Zellen 
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
		
		! Warten auf weiterdrücken
		READ *
		PRINT *, ""

	END SUBROUTINE print2D

END MODULE mod_print
