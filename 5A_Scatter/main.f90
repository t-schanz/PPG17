program scatter
    use mpi
    use initialize
    use calc
    
    implicit none
    integer :: rank, size, tag, master = 0, status(MPI_STATUS_SIZE), ierr 
    integer, dimension(:,:), allocatable :: matrix_big
    integer, dimension(:,:), allocatable :: matrix_chunk
    integer, parameter :: MATRIX_SIZE = 24
    integer :: chunk_size, scatter_size
    integer :: i,j, matrix_sum


    call MPI_INIT(ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)
        call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

        if (rank .eq. master) then
            if (mod(real(MATRIX_SIZE),real(size)) /= 0 ) then   !Fehlerabfrage
                print *, 'Die Anzahl der Prozessoren funktioniert nicht in Zusammenhang mit der gewaehlten', &
                        & ' Matrix. Versuchen Sie eine der folgenden Prozessorgroessen oder aendern sie die Groesse der Matrix:'

                do i=2,MATRIX_SIZE
                    if (mod(real(MATRIX_SIZE),real(i)) == 0) then
                        print *, i
                    endif
                enddo
                stop
            endif
        endif

        call createMatrix(matrix_big, MATRIX_SIZE,MATRIX_SIZE)
        call initMatrix(matrix_big)

        chunk_size = int(real(MATRIX_SIZE)/real(size))
        call createMatrix(matrix_chunk, MATRIX_SIZE, chunk_size)


        scatter_size = MATRIX_SIZE*chunk_size
        call MPI_SCATTER(matrix_big,scatter_size,MPI_INTEGER,matrix_chunk,scatter_size,MPI_INTEGER,master,MPI_COMM_WORLD,ierr)
        call computation(matrix_chunk,rank)

        !write(*,*) matrix_chunk
        
        call MPI_GATHER(matrix_chunk,scatter_size,MPI_INTEGER,matrix_big,scatter_size,MPI_INTEGER,master,MPI_COMM_WORLD,ierr)
        
        if (rank .eq. master) then            
            matrix_sum = sum(matrix_big)
            write(*,*) 'Gesamtsumme = ' ,matrix_sum
        endif


        
    call MPI_FINALIZE(ierr)
    

end program scatter
