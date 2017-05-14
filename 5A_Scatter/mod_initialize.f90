MODULE initialize
      implicit none

    CONTAINS
        !subroutine createMatrix
        !subroutine initMatrix          belegt die matrix mit den in der Aufgabe vorgegebenen Werten

	subroutine createMatrix(matrix, matrixSizeX, matrixSizeY)
		integer, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
                integer, intent(in) :: matrixSizeX , matrixSizeY ! Kantenlänge der Matrix

		!Zuweisen der Matrix
		allocate(matrix(matrixSizeX, matrixSizeY))
	end subroutine createMatrix


        subroutine initMatrix(matrix)
                integer, allocatable, dimension(:,:), intent(inout) :: matrix
                integer :: i,j,counter    !schleifenvariable
                counter = 1
                
                do j=lbound(matrix,2),ubound(matrix,2)
                    do i=lbound(matrix,1), ubound(matrix,1)
                        matrix(i,j) = counter
                        counter = counter + 1    
                    enddo
                enddo

        end subroutine initMatrix

end MODULE initialize

