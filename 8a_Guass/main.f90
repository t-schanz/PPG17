program poisson
        use mpi
	use initialize
        use run
	use finalize
	!
        implicit none
        !
	! parameter:
	integer, parameter :: edgeLength = 97	! Kantenlänge der gesamten Matrix
		! Nicht alle outputSizes funktionieren. 9 funktioniert.
        integer, parameter :: outputSize = 9	! Kantenlänge der ausgegebenen Matrix
	integer, parameter :: loopSize = 100	! Anzahl der Iterationen
	double precision, parameter :: abortPrecision = 0.0001 ! Abbruchkriterium
	!mpi:
	integer :: mpi_rank, mpi_size, tag, status(MPI_STATUS_SIZE), mpi_ierr !MPI
	integer :: master, last ! Besondere Prozess IDs
        ! data:
	double precision, dimension(:,:), allocatable :: matrix ! or whatever fits
	! auxiliary
	integer :: lineCount ! die Anzahl der Zeilen in der einzelnen Matrix
	integer :: offset ! = Index der ersten eigenen Zeile in Gesamtmatrix
        logical :: checkExit =.False., allExit = .false. ! Abbruchbedingung
	double precision :: diff
	integer :: i, j !Schleifenvariablen
	! für Ausgabe	
	integer :: interlines ! Abstand zwischen Zeilen, die ausgegeben werden sollen
	integer :: currentRow = 1 ! Laufender Zähler für Ausgabe über alle Prozesse
	interlines = edgeLength / (outputSize - 1)

	
	! INIT
	call MPI_INIT(mpi_ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_size,mpi_ierr)
        call MPI_COMM_RANK(MPI_COMM_WORLD,mpi_rank,mpi_ierr)
	master = 0
	last = mpi_size - 1
	! Initialisiert Variablen zur Orientierung in Gesamtmatrix
	call initLineCount(lineCount, offset, edgeLength, mpi_size, mpi_rank)
	! Eigene Matrix nach Bedarf
	call allocateMatrix(matrix, mpi_rank, mpi_size, lineCount, edgeLength)
	! Startbelegung der Matrix
	call initializeMatrix(matrix, mpi_rank, mpi_size, lineCount, offset)
	call MPI_BARRIER(MPI_COMM_WORLD,mpi_ierr)
	


	! CALC:
	do while (.not. allExit)
		do i=1, 100
			call calculate(matrix, mpi_rank, master, last, mpi_ierr, diff)
		end do
		if (diff <= 0.0000001) then !10**(-7)) then
			checkExit = .true.
		endif
		call mpi_barrier(mpi_comm_world, mpi_ierr)
		call mpi_reduce(checkExit, allExit, 1, mpi_logical, mpi_land, 0, mpi_comm_world, mpi_ierr)
		call mpi_bcast(allExit, 1, MPI_logical, 0, MPI_COMM_World, mpi_ierr)	
	end do
	call MPI_BARRIER(MPI_COMM_WORLD, mpi_ierr)
	

	! PRINT:
	interlines = edgeLength / (outputSize - 1)
	! master fängt an:
	if (mpi_rank == master) then
		! Prolog:
		print *, ""
		print *, "Fertige Matrix"
		! Druckt alle relevanten Zeilen in der Master-Matrix
		do while (currentRow < (offset + lineCount))
			call printRow(matrix, currentRow, interlines)
			currentRow = currentRow + interlines
		end do
		! Sagt dem nächsten bescheid
		call mpi_send(currentRow, 1, mpi_integer, 1, 2017, mpi_comm_world, mpi_ierr)
	else 
		! warten bis der staffelstab kommt
		call mpi_recv(currentRow, 1, mpi_integer, (mpi_rank - 1), 2017, mpi_comm_world, status, mpi_ierr)
		! dann alle relevanten zeilen drucken (wie master)
		do while (currentRow <= (offset + lineCount))
			call printRow(matrix, (currentRow - offset + 1), interlines)
			currentRow = currentRow + interlines
		end do
		! (falls nicht letzter) staffelstab weitergeben
		if (mpi_rank .ne. last) then
			call mpi_send(currentRow, 1, mpi_integer, (mpi_rank + 1), 2017, mpi_comm_world, mpi_ierr)		
		endif
	endif
	
	
	! FINAL
	deallocate(matrix)
        call MPI_FINALIZE(mpi_ierr)


end program poisson
