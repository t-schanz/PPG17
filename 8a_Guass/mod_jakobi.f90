module run
	!
	use mpi
	use finalize
	!
	implicit none
	!
	contains

	! Wendet den Jacobi Algorhytmus einmal auf eine Teilmatrix an, kommuniziert mit den Nachbarn
	subroutine calculate(matrix, mpi_rank, mpi_master, mpi_last, mpi_ierr, diff)
		double precision, allocatable, dimension(:,:), intent(inout) :: matrix !matrix
		double precision, allocatable, dimension(:,:) :: temp !temporäre Kopie
		! MPI:
		integer, intent(in):: mpi_rank, mpi_master, mpi_last, mpi_ierr
		integer :: status(MPI_STATUS_SIZE)
		! aux:
		double precision, intent(inout) :: diff ! größte Differenz zu Vorwert
		double precision :: cur_diff ! Zwischenspeicher für Differenz
		double precision, allocatable, dimension(:) :: first, last ! Briefumschläge
		double precision :: star, corr !Hilfsvariablen
		integer :: request1, request2  !Hilfsvariablen
		integer :: i, j !Schleifenvariablen

		! Alloziieren:
		allocate(temp(ubound(matrix, 1), ubound(matrix, 2)))
		allocate(first(ubound(matrix, 2)))
		allocate(last(ubound(matrix, 2)))


		! Erste Reihe ausrechnen
		call calculateRow(matrix, temp, 2)
		! Und (falls nicht master) auch versenden
		if (mpi_rank /= mpi_master) then
			first = matrix(2, :)
			call mpi_isend(first, size(first), mpi_double, (mpi_rank - 1), 2002, &
					mpi_comm_world, request1, mpi_ierr)
		endif
		
		! Letzte Zeile ausrechnen
		call calculateRow(matrix, temp, (ubound(matrix,1) - 1))
		! Und (falls nicht last) auch versenden
		if (mpi_rank /= mpi_last) then 
			last = matrix(ubound(matrix, 1), :)
			call mpi_isend(last, size(last), mpi_double, (mpi_rank + 1), 2005, &
					mpi_comm_world, request2, mpi_ierr)
		endif
		

		! Den ganzen Rest ausrechnen.
		do j = 2, ubound(matrix, 1)-2
			call calculateRow(matrix, temp, j)
		end do


		! Wenn last übertragen der letzen zeile
		if (mpi_rank == mpi_last) then
			temp(ubound(matrix,1),:) = matrix(ubound(matrix,1),:)
		! Sonst empfangen
		else
			call MPI_IRECV(first, size(first), MPI_DOUBLE, (mpi_rank + 1), 2002, &
			& MPI_COMM_WORLD, request1, mpi_ierr)
			temp(ubound(matrix,1),:) = first(:)
		end if

		! Wenn master übertragen der ersten Zeile
		if (mpi_rank == mpi_master) then
			temp(1,:) = matrix(1,:)	
		! Sonst empfangen
		else
			call MPI_IRECV(last, size(last), MPI_DOUBLE, mpi_rank-1, 2005, &
			& MPI_COMM_WORLD, request2, mpi_ierr)
			temp(1,:) = last(:)
		end if

		
		! Versichern, dass alles funktioniert hat
		call mpi_wait(request1, status, mpi_ierr)
		call mpi_wait(request2, status, mpi_ierr)

		
		diff = 0
		do i = 1, ubound(matrix,1)
			do j = 1, ubound(matrix,2)
				cur_diff = temp(i,j) - matrix(i,j)
				if (cur_diff > diff) then
					diff = cur_diff
				endif
			end do
		end do


		! Übertrag
		matrix = temp
		! Speicher freigeben
		deallocate(temp)
		deallocate(first)
		deallocate(last)

	end subroutine  calculate


	! Errechnet eine neue Zeile und trägt sie in temp ein.
	subroutine calculateRow(matrix,temp, j)
		double precision, allocatable, dimension(:,:), intent(in) :: matrix !matrix
		double precision, allocatable, dimension(:,:), intent(inout) :: temp !temporäre Kopie
		integer, intent(in) :: j ! Zeilennummer auf der gerechnet wird

		double precision :: star, corr !Hilfsvariable
		integer :: i !Schleifenvariablen

		! Ränder:
		temp(j,1) = matrix(j,1)
		temp(j,ubound(matrix,2)) = matrix(j,ubound(matrix,2))

		! Rechnet für eine Zeile
		do i = lbound(matrix, 2)+1, ubound(matrix, 2)-1
			! Stern
			star = -matrix(j,i+1)-matrix(j-1,i)+4*matrix(j,i)-matrix(j+1,i)-matrix(j,i-1)
			! Korrektur
			corr = - star/4
			! Übertrag
			temp(j,i) = matrix(j,i) + corr
		end do
	end subroutine  calculateRow

end module run
