MODULE run
	IMPLICIT NONE
	CONTAINS
	
	subroutine calculate(matrix,temp)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		double precision, allocatable, dimension(:,:), intent(inout) :: temp !temporäre Kopie
		double precision :: star, corr, h !Hilfsvariable
!                integer :: diff, counter !Zähler in Schleifen
!                logical, intent(inout) :: checkExit
		integer :: i, j !Schleifenvariablen
		allocate(temp(ubound(matrix, 1), ubound(matrix, 2)))
		h = 1.0 / (ubound(matrix, 1) - lbound(matrix, 1))
        
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
		
        end subroutine  calculate




        subroutine writeTempEdges(matrix,temp,mpi_rank,master,last)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		double precision, allocatable, dimension(:,:), intent(inout) :: temp !temporäre Kopie
                integer, intent(in) :: mpi_rank,master,last
		integer :: i, j !Schleifenvariablen
                integer :: width, height

!                temp(:,:) = mpi_rank +1

		!Belegen der Ränder (bleiben unverändert)
		j = lbound(matrix,2)
                width = ubound(matrix,1) 
                height = ubound(matrix,2)
		do i = lbound(matrix, 1), width
                        temp(i,j) = matrix(i,j) ! oben                        
                        temp(i,height) = matrix(i,height) ! unten

		end do
                i = lbound(matrix,1)
!                write(*,*) i, width
                do j=lbound(matrix,2), height
			temp(i,j) = matrix(i,j) ! links
			temp(width,j) = matrix(width,j) ! rechts
                enddo
	end subroutine writeTempEdges




        subroutine writeTempOnMatrix(matrix,temp)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		double precision, allocatable, dimension(:,:), intent(inout) :: temp !temporäre Kopie

                matrix = temp

                deallocate(temp)

        end subroutine writeTempOnMatrix

!        subroutine checkPrecision()
!                diff = 0
!                counter = 0
!                
!                !checken, ob die Lösung schon genau genug ist:
!		do j = lbound(matrix, 2)+1, ubound(matrix, 2)-1
!		        do i = lbound(matrix, 1)+1, ubound(matrix, 1)-1
!                                ! (1) Wenn der neu berechnete Wert weniger als 10e-6 von dem alten abweicht:
!                                if (abs(temp(i,j) -matrix(i,j)) <= 1e-6 ) then
!                                        diff = diff +1
!                                endif
!                                counter = counter +1
!                        enddo
!                enddo
!                
!                !Wenn (1) für alle Werte in der Matrix zutrifft:
!                if (diff == counter) then
!                        checkExit = .True.  !beendet die Schleife in poission.f90
!                endif
!		
!                ! Übertrag
!		matrix = temp   !Selbst wenn die Abbruchbedingung erfüllt ist, so muss der bereits berechnete Wert
!                                !ja trotzdem nicht weggeworfen werden.
!		deallocate(temp)
!	end subroutine

END MODULE run


