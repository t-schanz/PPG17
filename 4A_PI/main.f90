program calc_pi
    use mpi
    use mod_calculate

    implicit none
    double precision :: pi =0, message, pi_sum=0
    real :: preci = 1e9
    integer :: ierr, master, rank,size,tag,status(MPI_STATUS_SIZE)
    real :: start,ende
    integer :: i

    call MPI_INIT(ierr)
                master = 0    
                call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
                call MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)
                !write(*,*) rank,size
                if (rank.ne.master) then
                    pi = 0
                    start = real(rank-1)/real(size-1)
                    ende = real(rank)/real(size-1)
                    !write(*,*) start, ende

                    call func(start, ende,pi,preci)
                    call MPI_SEND(pi,1,MPI_DOUBLE_PRECISION,master,2017,MPI_COMM_WORLD,ierr)
                else
                    
                    do i=1, size-1
                        call MPI_RECV(pi,1,MPI_DOUBLE_PRECISION,i,2017,MPI_COMM_WORLD,status,ierr)
                        pi_sum = pi_sum + pi
                        !write(*,*) pi, pi_sum
                    enddo
            
                endif
    call MPI_FINALIZE(ierr)
    
    print *, pi_sum


end program calc_pi
