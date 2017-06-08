program poisson
        use mpi
	use initialize
!        use run
	use finalize
	!
        implicit none
        !
	! parameter:
	integer, parameter :: edgeLength = 184	! Kantenlänge der gesamten Matrix
! gewisse outputsiyes machen probleme
! vor allem gerade
! 3, 5, 9 ist ok.
        integer, parameter :: outputSize = 5	! Kantenlänge der ausgegebenen Matrix
	integer, parameter :: loopSize = 40000	! Anzahl der Iterationen
	! mpi:
	integer :: mpi_rank, mpi_size, tag, status(MPI_STATUS_SIZE), mpi_ierr !MPI
	integer :: master, last ! Besondere Prozess IDs
        ! data:
	double precision, dimension(:,:), allocatable :: matrix ! or whatever fits
	! auxiliary
	integer :: lineCount ! die Anzahl der Zeilen in der einzelnen Matrix
	integer :: offset ! = Index der ersten eigenen Zeile in Gesamtmatrix
        logical :: checkExit =.False. ! Abbruchbedingung
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
	
	! DEBUGGING PRINT
!	if (mpi_rank == master) then 
!		call printNumber(smallestStep)
!		print *, ""
!		print *, mpi_rank, "l", matrix(1,1), matrix(1,edgeLength)
!	else
!		print *, mpi_rank, "l", matrix(2,1), matrix(1,edgeLength)
!	endif
!	if (mpi_rank == last) then 
!		print *, mpi_rank, "b", matrix(ubound(matrix,1) -1 , 1), matrix(ubound(matrix,1) - 1, edgeLength)
!		print *, mpi_rank, "u", matrix(ubound(matrix,1), 1), matrix(ubound(matrix,1), edgeLength)
!	else
!		print *, mpi_rank, "u", matrix(ubound(matrix,1) - 1, 1) + smallestStep, matrix(ubound(matrix,1) -1, edgeLength)
!	endif
	
	
	! PROLOG
	interlines = edgeLength / (outputSize - 1)
	if (mpi_rank == master) then
		! Prolog:
		print *, ""
		print *, "Fertige Matrix"
		! Erste  Zeile seperat
		call printRow(matrix, currentRow, interlines)
		currentRow = interlines
		! Dann alles andere (eventuell)
		do while (currentRow < (offset + lineCount))
			call printRow(matrix, currentRow, interlines)
			currentRow = currentRow + interlines
		end do
		call mpi_send(currentRow, 1, mpi_integer, 1, 2017, mpi_comm_world, mpi_ierr)
	else 
		call mpi_recv(currentRow, 1, mpi_integer, (mpi_rank - 1), 2017, mpi_comm_world, status, mpi_ierr)
		
		do while (currentRow <= (offset + lineCount))
!			print *, mpi_rank, "printing:", currentRow, "d.h.", currentRow - offset + 1, "of", ubound(matrix, 1)
			call printRow(matrix, (currentRow - offset + 1), interlines)
			currentRow = currentRow + interlines
		end do
		if (mpi_rank .ne. last) then
			call mpi_send(currentRow, 1, mpi_integer, (mpi_rank + 1), 2017, mpi_comm_world, mpi_ierr)		
		endif
	endif

	
	! FINAL
	deallocate(matrix)
        call MPI_FINALIZE(mpi_ierr)

end program poisson

