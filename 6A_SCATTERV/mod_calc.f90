MODULE calc
    implicit none
    CONTAINS
    !subroutine computation

    subroutine computation(matrix,rank,matrix_sum)
        integer, dimension(:,:), intent(inout), allocatable :: matrix
        integer, intent(in) :: rank
        integer :: i,j
        integer,intent(inout) :: matrix_sum

        do j=lbound(matrix,2), ubound(matrix,2)
            do i=lbound(matrix,1), ubound(matrix,1)
                matrix(i,j) = matrix(i,j) *(rank +1)           
            enddo
        enddo
        matrix_sum = sum(matrix)
        write(*,*) 'Summe von Prozessor ', rank, ' = ', matrix_sum 
    end subroutine


END MODULE

