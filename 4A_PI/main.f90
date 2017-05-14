program calc_pi
	use mpi
	use mod_calculate


	implicit none

	double precision :: Abschnitt = 0, message, piSumme = 0
	real :: anzahlStuetzpunkte = 1e9
	integer :: ierr, rank, size, tag, status(MPI_STATUS_SIZE)
	integer, parameter :: master = 0 !Masterprozess festgelegt
	real :: lowerBoundary, upperBoundary !untere und Obere Grenze für die Integrale
	integer :: i !Schleifenvariable

	! performance measurement:
	real :: startTime, endTime
	double precision, parameter :: pibel = 3.1415926535897932 ! Pi aus von pible.de zur Messung der Genauigkeit
	call cpu_time(starttime)

	call MPI_INIT(ierr)
	! Rank und Size in Erfahrung bringen
	call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)

	if (rank .ne. master) then
		Abschnitt = 0
		lowerBoundary = real(rank-1)/real(size-1)
		upperBoundary = real(rank)/real(size-1)
		call func(lowerBoundary, upperBoundary, Abschnitt, anzahlStuetzpunkte)
		call MPI_SEND(Abschnitt,1,MPI_DOUBLE_PRECISION,master,2017,MPI_COMM_WORLD,ierr)
	else
		do i = 1, size -1
			call MPI_RECV(Abschnitt,1,MPI_DOUBLE_PRECISION,i,2017,MPI_COMM_WORLD,status,ierr)
			piSumme = piSumme + Abschnitt
		enddo

		! performance measurement:
		call cpu_time(endtime)
		! Ausgabe
		print *, "Errechneter Wert:       ", piSumme
		print *, "Zeit für diese Rechnung:", (endtime - starttime), "Sekunden"
		print *, "Abweichung von Pi:      ", (pibel - piSumme)
		print *, "(Referenzwert für Pi von pibel.de)"
	endif
	call MPI_FINALIZE(ierr)


end program calc_pi
