MODULE run
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE calculate(matrix,checkExit) !more parameters as needed
		IMPLICIT NONE
		double precision, dimension(:,:), intent(inout) :: matrix
                logical, intent(inout) :: checkExit
                double precision :: star, corr, h
                integer :: diff, counter
                integer :: i,j !Schleifenvariablen
		! calculate with Gauß-Seidel Method

		h = 1.0 / (ubound(matrix, 1) - lbound(matrix, 1))
                diff = 0
                counter = 0

        !$omp parallel do default(none) &
        !$omp shared(star,corr,matrix,h,diff,counter) private(i,j)
                ! Rechnet nicht an den Rändern
		do j = lbound(matrix, 2)+1, ubound(matrix, 2)-1
		        do i = lbound(matrix, 1)+1, ubound(matrix, 1)-1
				! Stern
				star = -matrix(i,j+1)-matrix(i-1,j)+4*matrix(i,j)-matrix(i+1,j)-matrix(i,j-1)
				! Korrektur
				corr = matrix(i,j) * h * h - star/4
				! Übertrag
                                if (corr <= 10e-6) then
                                        diff = diff +1
                                endif
                                counter = counter +1
				matrix(i,j) = matrix(i,j) + corr
			end do
		end do

        !$omp end parallel do
		
                if (diff == counter) then
                    checkExit = .True.
                endif

	END SUBROUTINE calculate

END MODULE run
