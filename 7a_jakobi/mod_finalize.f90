MODULE finalize
!	use mpi
	IMPLICIT NONE
	CONTAINS
	
	! subroutine printNumber	Ausgabe einzelnes Double auf Konsole
	! subroutine printRow		Ausgabe einer Zeile auf Konsole
	
	
	! gibt einzelnen Wert auf Konsole aus
	subroutine printNumber(x)
		double precision, intent(in) :: x
		
		! Typumwandlung zu Real um Nachkommastellen zu schneiden
		write (*,1, advance='no') real(x)
		! f10.8 heisst: 10 Stellen für die ganze Zahl, 8 für Nachkommastellen
		1	format(f10.8)
		! Abstand
		write (*, '(A)', advance='no') "  "
	end subroutine printNumber
	
	
	! Prints one Row, only one Number every <interlines> rows
	subroutine printRow(matrix, row, interlines)
		double precision, dimension(:,:), allocatable, intent(in) :: matrix
		integer, intent(in) :: row ! Row to be printed
		integer, intent(in) :: interlines ! Size of gap between printed numbers
		integer :: i ! Schleifenvariable

		
!		write (*, 2, advance='no') row
!		2	format (i2)
!		write (*, 1, advance='no')
!		1	format (": ")
		call printNumber(matrix(row,1))
		do i = 1, ubound(matrix, 2)
			if (mod(i, interlines) == 0) then
				call printNumber(matrix(row,i))
			endif
		end do
		print *, ""
	end subroutine printRow
	
	
	! Lässt die einzelnen Prozesse nacheinander Ihre Lines ausgeben.
!	subroutine printMatrix(matrix, edgeLength, outputSize, lineCount, offset, mpi_rank, mpi_size status, mpi_ierr)
!		double precision, dimension(:,:), allocatable, intent(in) :: matrix
!		integer, intent(in) :: edgeLength
!		integer, intent(in) :: outputSize
!		integer, intent(in) :: lineCount
!		integer, intent(in) :: offset
!		! mpi:
!		integer, intent(in) :: mpi_rank
!		integer, intent(in) :: mpi_size
!		integer, intent(in) :: status
!!		integer, intent(in) :: mpi_comm_world
!		integer, intent(in) :: mpi_ierr
!		integer :: master, last
!		! aux:
!		character :: prolog = "Fertige Matrix"
!		integer :: interlines ! Abstand zwischen Zeilen, die ausgegeben werden sollen
!		integer :: currentRow = 1 ! Laufender Zähler für Ausgabe über alle Prozesse
!		interlines = edgeLength / (outputSize - 1)
!		master = 0
!		last = mpi_size - 1
!		
!	
!		if (mpi_rank == master) then
!			! Prolog:
!			print *, ""
!			print *, prolog
!			! Erste  Zeile seperat
!			call printRow(matrix, currentRow, interlines)
!			currentRow = interlines
!			! Dann alles andere (eventuell)
!			do while (currentRow < (offset + lineCount))
!				call printRow(matrix, currentRow, interlines)
!				currentRow = currentRow + interlines
!			end do
!			call mpi_send(currentRow, 1, mpi_integer, 1, 2017, mpi_comm_world, mpi_ierr)
!		else 
!			call mpi_recv(currentRow, 1, mpi_integer, (mpi_rank - 1), 2017, mpi_comm_world, status, mpi_ierr)
!			do while (currentRow <= (offset + lineCount))
!!				print *, mpi_rank, "printing:", currentRow, "d.h.", currentRow - offset + 1, "of", ubound(matrix, 1)
!				call printRow(matrix, (currentRow - offset + 1), interlines)
!				currentRow = currentRow + interlines
!			end do
!			if (mpi_rank .ne. last) then
!				call mpi_send(currentRow, 1, mpi_integer, (mpi_rank + 1), 2017, mpi_comm_world, mpi_ierr)		
!			endif
!		endif
!	end subroutine printMatrix


END MODULE finalize

