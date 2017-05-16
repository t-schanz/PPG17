program visualisierung

	use mpi

	implicit none

	character(len=80) :: string !Für Afgabenteil 1
	integer :: ierr, rank, size, tag, status(MPI_STATUS_SIZE) ! für MPI
	integer, parameter :: master = 0 !Masterprozess festgelegt
	integer :: i !Schleifenvariable
	integer :: single_int =5   !Für Aufgabenteil 2 und 3

	call MPI_INIT(ierr)
	! Rank und Size in Erfahrung bringen
	call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)


	! Part 1: Send and Receive
	if (rank .eq. master) then
		write(*,*)
		write(*,*) "Part 1: Send and Receive"
		do i = 1, (size - 1)
			call MPI_RECV(string,80,MPI_CHARACTER,i,1,MPI_COMM_WORLD,status,ierr)
			print *, string
		enddo
		write(*,*) "Hello from master"
	else
		write(string,"('Hello from',I3)") rank
		call MPI_SEND(string,80,MPI_CHARACTER,master,1,MPI_COMM_WORLD,ierr)
	endif
	call MPI_BARRIER(MPI_COMM_WORLD,ierr)


	! Part 2: Broadcast
	if (rank .eq. master) then
		write(*,*)
		write(*,*) "Part 2: Broadcast"
		single_int = 2
	endif
	call MPI_BCAST(single_int,1,MPI_INTEGER,master,MPI_COMM_WORLD,ierr)
	write(*,*) "Process: ", rank, "broadcasted int:", single_int
	call MPI_BARRIER(MPI_COMM_WORLD,ierr)


	! Part 3: Simulated Broadcast (Send and Receive)
	if (rank .eq. master) then
		write(*,*)
		write(*,*) "Part 3: Simulated Broadcast (Send and Receive)"
		single_int = 3
		do i = 1, (size - 1)
			call MPI_SEND(single_int,1,MPI_INTEGER,i,3,MPI_COMM_WORLD,ierr)
		enddo
	else
		call MPI_RECV(single_int,1,MPI_INTEGER,master,3,MPI_COMM_WORLD,status,ierr)
	endif
	write(*,*) "Process: ", rank, "broadcasted int:", single_int


	call MPI_FINALIZE(ierr)
end program visualisierung
