PROGRAM Poisson
	USE initialize
	USE run
	USE finalize
	IMPLICIT NONE
	!
	double precision, dimension(:,:), allocatable :: matrix ! or whatever fits
	integer, parameter :: outputSize = 9
	integer :: i 

	call createMatrix(matrix)
	call initializeMatrix(matrix)
	
	do i = 0, 40000
		print *, "run ", i
		call calculate(matrix)
	end do
	
	call outputMatrix(matrix, outputSize)

	call freeMatrix(matrix)



	
END PROGRAM Poisson
