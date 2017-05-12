program calc_pi
    use mpi
    use mod_calculate

    implicit none
    double precision :: pi =0, message, pi_sum=0
    real :: preci = 1e9 !Setzt die Anzahl der Stützstellen
    integer :: ierr, master, rank,size,tag,status(MPI_STATUS_SIZE)  !Alles was MPI so benötigt
    real :: start,ende  !Bestimmt die Größe des Intervalls pro Processor
    integer :: i    !Schleifenvariable

    call MPI_INIT(ierr)
                master = 0    !Setzt den Prozessor mit rank=0 als master
                call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
                call MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)
                if (rank.ne.master) then
                    pi = 0
                    start = real(rank-1)/real(size-1)
                    ende = real(rank)/real(size-1)

                    call func(start, ende,pi,preci) !Gibt das Intervall und die Anzahl der 
                                                    !Stüzstellens an die subroutine weiter
                    call MPI_SEND(pi,1,MPI_DOUBLE_PRECISION,master,2017,MPI_COMM_WORLD,ierr)
                else
                    
                    do i=1, size-1
                        call MPI_RECV(pi,1,MPI_DOUBLE_PRECISION,i,2017,MPI_COMM_WORLD,status,ierr)
                        pi_sum = pi_sum + pi    !summiert die Fläche aller Berechnungen auf

                    enddo
                    
                    print *, pi_sum !das Print an dieser Stelle stellt sicher, dass es nur 
                                    !einmal ausgeführt wird.
            
                endif
    call MPI_FINALIZE(ierr)    

end program calc_pi
