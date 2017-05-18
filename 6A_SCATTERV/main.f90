program scatter
    use mpi
    use initialize
    use calc
    
    implicit none
    integer :: rank, size, tag, master = 0, status(MPI_STATUS_SIZE), ierr 
    integer, dimension(:,:), allocatable :: matrix_big
    integer, dimension(:,:), allocatable :: matrix_chunk
    integer, parameter :: MATRIX_SIZE = 24
    integer, dimension(:), allocatable :: sendcounts, displacement
    integer :: i,j, matrix_sum, chunk_sum, chunk_sum_all =0


    call MPI_INIT(ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)
        call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

        !if (rank .eq. master) then  !Der Master initialisiert die Hauptmatrix
                call createMatrix(matrix_big, MATRIX_SIZE,MATRIX_SIZE)
                call initMatrix(matrix_big)
        !endif

        !if (rank .eq. 1) then   !nebenbei initialisiert proc1 sendcounts und displacement
            call initSendcounts(sendcounts, MATRIX_SIZE,size)
            call initDisplacement(displacement,sendcounts)

        !endif
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        !write(*,*) displacement
        !write(*,*) sendcounts
        call createMatrix(matrix_chunk, MATRIX_SIZE, sendcounts(rank+1))
        
        call MPI_SCATTERV(matrix_big,sendcounts,displacement,MPI_INTEGER,&
                        & matrix_chunk,sendcounts(rank+1),MPI_INTEGER,master,MPI_COMM_WORLD,ierr)

        call computation(matrix_chunk,rank,chunk_sum)
        
        call MPI_GATHERV(matrix_chunk,sendcounts(rank+1),MPI_INTEGER, &
                        & matrix_big,sendcounts,displacement,MPI_INTEGER,master,MPI_COMM_WORLD,ierr)

        call MPI_REDUCE(chunk_sum,chunk_sum_all,1,MPI_INTEGER,MPI_SUM,master,MPI_COMM_WORLD,ierr)
        
        if (rank .eq. master) then            
            matrix_sum = sum(matrix_big)
            write(*,*) 'Gesamtsumme = ' ,matrix_sum
            write(*,*) 'Gesamtsumme aus den Teilsummen:', chunk_sum_all 
            deallocate(matrix_big)
        endif

        deallocate(matrix_chunk)


            
    call MPI_FINALIZE(ierr)
    
end program scatter
