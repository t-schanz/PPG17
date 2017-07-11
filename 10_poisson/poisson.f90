PROGRAM Poisson
    USE omp_lib
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
                    write(*,*) 'Abbruchbedingung nach ' , i+1 , 'Durchläufen erfüllt.'  !i+1 da bei 0 angefangen
                                                                                        !wird zu zählen.
                    exit
                endif
	end do
	
	call outputMatrix(matrix, outputSize)

	call freeMatrix(matrix)



	
END PROGRAM Poisson
