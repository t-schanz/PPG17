MODULE initialize
      implicit none

    CONTAINS
        !subroutine createMatrix
        !subroutine initMatrix          belegt die matrix mit den in der Aufgabe vorgegebenen Werten
        !subroutine initSendcounts
        !subroutine initDisplacement

	subroutine createMatrix(matrix, matrixSizeX, matrixSizeY)
		integer, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
                integer, intent(in) :: matrixSizeX , matrixSizeY ! Kantenlänge der Matrix

		!Zuweisen der Matrix
		allocate(matrix(matrixSizeX, matrixSizeY))
	end subroutine createMatrix


        subroutine initMatrix(matrix)
                integer, allocatable, dimension(:,:), intent(inout) :: matrix
                integer :: i,j,counter    !schleifenvariable
                counter = ubound(matrix,1) *ubound(matrix,2)
                
                do j=lbound(matrix,2),ubound(matrix,2)
                    do i=lbound(matrix,1), ubound(matrix,1)
                        matrix(i,j) = counter
                        counter = counter - 1     
                    enddo
                enddo

        end subroutine initMatrix

        subroutine initSendcounts(sendcounts,matrixSize,size)
                integer, dimension(:), allocatable, intent(inout) :: sendcounts
                integer, intent(in) :: matrixSize, size
                integer :: i, last, other

                allocate(sendcounts(size))

                last = mod(matrixSize,size)
                other = real(matrixSize-last)/real(size)

                do i=1,size-1
            
                        sendcounts(i) = other
                enddo
                sendcounts(size) = last + other
                !write(*,*) sendcounts

        end subroutine initSendcounts

        subroutine initDisplacement(displacement,sendcounts)
                integer, dimension(:), allocatable,intent(inout) :: displacement
                integer, dimension(:), allocatable, intent(in) :: sendcounts
                integer :: i

                allocate(displacement(ubound(sendcounts,1)))
                displacement(1) = 0

                do i=2,ubound(sendcounts,1)
                        displacement(i) = sendcounts(i-1) + displacement(i-1)
                enddo
                !write(*,*) displacement


        end subroutine initDisplacement

end MODULE initialize

