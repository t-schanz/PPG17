program calc_pi
	use mpi


	implicit none

        character(len=80) :: string !Für Afgabenteil 1
	integer :: ierr, rank, size, tag, status(MPI_STATUS_SIZE)
	integer, parameter :: master = 0 !Masterprozess festgelegt
	integer :: i !Schleifenvariable
        integer :: single_int =5   !Für Aufgabenteil 2


	call MPI_INIT(ierr)
	! Rank und Size in Erfahrung bringen
	    call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
	    call MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)

	    if (rank .ne. master) then
                write(string,"('Hello from',I3)") rank 
		call MPI_SEND(string,80,MPI_CHARACTER,master,1,MPI_COMM_WORLD,ierr)
	    else
		do i = 1, size -1
			call MPI_RECV(string,80,MPI_CHARACTER,i,1,MPI_COMM_WORLD,status,ierr)
                        print *, string
		enddo

	    endif

            call MPI_BARRIER(MPI_COMM_WORLD,ierr)


            if (rank .eq. master) then
                single_int = 6
            endif

            call MPI_BCAST(single_int,1,MPI_INTEGER,master,MPI_COMM_WORLD,ierr)

            if (rank .ne. master) then
                write(*,*) single_int
            endif

            call MPI_BARRIER(MPI_COMM_WORLD,ierr)

            if (rank .eq. master) then
                single_int = 7
                do i=1,size-1
                    call MPI_SEND(single_int,1,MPI_INTEGER,i,3,MPI_COMM_WORLD,ierr)
                enddo
            else
                call MPI_RECV(single_int,1,MPI_INTEGER,master,3,MPI_COMM_WORLD,status,ierr)
                write(*,*) single_int
            endif



	call MPI_FINALIZE(ierr)


end program calc_pi
