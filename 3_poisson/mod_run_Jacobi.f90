MODULE run
	IMPLICIT NONE
	CONTAINS
	
	subroutine calculate(matrix)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		double precision, allocatable, dimension(:,:) :: temp !temporäre Kopie
		double precision :: star, corr, h !Hilfsvariable
		integer :: i, j !Schleifenvariablen
		allocate(temp(ubound(matrix, 1), ubound(matrix, 2)))
		h = 1.0 / (ubound(matrix, 1) - lbound(matrix, 1))
        
		! Rechnet nicht an den Rändern
		do i = lbound(matrix, 1)+1, ubound(matrix, 1)-1
			do j = lbound(matrix, 2)+1, ubound(matrix, 2)-1
				! Stern
				star = (matrix(i,j) * 4)
				star = star + matrix((i - 1),j)
				star = star + matrix((i + 1),j)
				star = star + matrix(i,(j - 1))
				star = star + matrix(i,(j + 1))
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
		
		! Übertrag
		matrix = temp
		
	end subroutine

END MODULE run


