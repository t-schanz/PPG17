MODULE run
    USE omp_lib
	IMPLICIT NONE
	CONTAINS
	
	subroutine calculate(matrix,checkExit)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		double precision, allocatable, dimension(:,:) :: temp !temporäre Kopie
		double precision :: star, corr, h !Hilfsvariable
                integer :: diff, counter !Zähler in Schleifen
                logical, intent(inout) :: checkExit
		integer :: i, j !Schleifenvariablen
		allocate(temp(ubound(matrix, 1), ubound(matrix, 2)))
		h = 1.0 / (ubound(matrix, 1) - lbound(matrix, 1))

        !$omp parallel do default(none)&
        !$omp shared(diff,counter,star,corr,h,temp,matrix) private(i,j)
        
		! Rechnet nicht an den Rändern
		do j = lbound(matrix, 2)+1, ubound(matrix, 2)-1
		        do i = lbound(matrix, 1)+1, ubound(matrix, 1)-1
				! Stern
				star = -matrix(i,j+1)-matrix(i-1,j)+4*matrix(i,j)-matrix(i+1,j)-matrix(i,j-1)
				! Korrektur
				corr = matrix(i,j) * h * h - star/4
				! Übertrag
				temp(i,j) = matrix(i,j) + corr
			end do
		end do
		
		!Belegen der Ränder (bleiben unverändert)
		i = 1
		do j = lbound(matrix, 1), ubound(matrix, 1)
			temp(i,j) = matrix(i,j) ! oben
			temp(j,i) = matrix(j,i) ! links
		end do
		j = ubound(matrix, 1)
		do i = ubound(matrix, 1), lbound(matrix, 1), -1
			temp(i,j) = matrix(i,j) ! rechts
			temp(j,i) = matrix(j,i) ! unten
		end do
		
                diff = 0
                counter = 0
        !$omp end parallel do
                
                !checken, ob die Lösung schon genau genug ist:
		do j = lbound(matrix, 2)+1, ubound(matrix, 2)-1
		        do i = lbound(matrix, 1)+1, ubound(matrix, 1)-1
                                ! (1) Wenn der neu berechnete Wert weniger als 10e-6 von dem alten abweicht:
                                if (abs(temp(i,j) -matrix(i,j)) <= 10e-6 ) then
                                        diff = diff +1
                                endif
                                counter = counter +1
                        enddo
                enddo
                
                !Wenn (1) für alle Werte in der Matrix zutrifft:
                if (diff == counter) then
                        checkExit = .True.  !beendet die Schleife in poission.f90
                endif
		
                ! Übertrag
		matrix = temp   !Selbst wenn die Abbruchbedingung erfüllt ist, so muss der bereits berechnete Wert
                                !ja trotzdem nicht weggeworfen werden.
		deallocate(temp)
	end subroutine

END MODULE run


