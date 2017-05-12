program scatter
    use mpi
    use initialize
    
    implicit none
    integer :: rank, size, tag, master = 0, status(MPI_STATUS_SIZE), ierr 
    integer, dimension(:,:), allocatable :: matrix_big
    integer, dimension(:,:), allocatable :: matrix_chunk
    integer, parameter :: MATRIX_SIZE = 24


    call MPI_INIT(ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)
        call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

        if (rank .eq. master) then
            call createMatrix(matrix_big, MATRIX_SIZE,MATRIX_SIZE)
            call initMatrix(matrix_big)

            write(*,*) matrix_big
        endif



        
    call MPI_FINALIZE(ierr)
    

end program scatter
