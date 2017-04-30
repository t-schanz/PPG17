PROGRAM Poisson
	USE initialize
	USE run
	USE finalize
	IMPLICIT NONE
	!
	double precision, dimension(:,:), allocatable :: matrix ! or whatever fits
	integer, parameter :: outputSize = 9,loopSize = 40000
	integer :: i 
        logical :: checkExit =.False.
	call createMatrix(matrix)
	call initializeMatrix(matrix)
	
	do i = 0, loopSize
		!print *, "run ", i
		call calculate(matrix,checkExit)
                if (checkExit) then
                    write(*,*) 'Abbruchbedingung nach ' , i , 'Durchläufen erfüllt.'
                    exit
                endif
	end do
	
	call outputMatrix(matrix, outputSize)

	call freeMatrix(matrix)



	
END PROGRAM Poisson
