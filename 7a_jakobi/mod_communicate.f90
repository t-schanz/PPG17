MODULE comm
        use MPI

        implicit none
        contains


        subroutine sendrec(temp,mpi_rank, master, last,status)
		double precision, allocatable, dimension(:,:),intent(inout) :: temp    
		double precision, allocatable, dimension(:) :: firstRow, lastRow, lastRowTemp
                integer, intent(in) :: mpi_rank, master, last
                integer, intent(inout) :: status
                integer :: mpi_ierr
                integer :: prev, next !Prozessoren mit denen kommuniziert wird


                !Schreibe die Teile der Matrix welche übergeben werden
                !in extra Arrays:
                allocate(firstRow(ubound(temp,2)))
                firstRow = temp(1,:)

                allocate(lastRow(ubound(temp,2)))
                lastRow = temp(ubound(temp,2),:)

                allocate(lastRowTemp(ubound(temp,2)))

                !Berechne die Partner mit denen Kommuniziert wird:
                prev = mpi_rank - 1
                next = mpi_rank + 1
                
                if (mpi_rank == last) next = master
                if (mpi_rank == master) prev = last

!                write(*,*) temp(1,:)

                !Verschicke die erste Zeile an den Vorgänger und erhalte
                !die letzte Zeile vom Nachfolger
		call MPI_SENDRECV(firstRow,size(firstRow),MPI_DOUBLE,&
		&	     prev,mpi_rank,lastRowTemp,size(lastRow),MPI_DOUBLE,&
		&	     next,next,MPI_COMM_WORLD,status,mpi_ierr)

                !Verschicke die letzte Zeile an den Nachfolger und
                !erhalte die erste Zeile vom Vorgänger
		call MPI_SENDRECV(lastRow,size(lastRow),MPI_DOUBLE,&
		&	     next,mpi_rank,firstRow,size(firstRow),MPI_DOUBLE,&
		&	     prev,prev,MPI_COMM_WORLD,status,mpi_ierr)

                !Der Master hat eine fixe erste Zeile
                if (mpi_rank /= master) temp(1,:) = firstRow

                !Der letzte Prozessor hat eine fixe letzte Zeile
		if (mpi_rank /= last) temp(ubound(temp,2),:) = lastRowTemp

                !Speicher aufräumen:
                deallocate(firstRow)
                deallocate(lastRow)
                deallocate(lastRowTemp)

        end subroutine sendrec




end MODULE comm
