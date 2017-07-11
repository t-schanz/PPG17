MODULE initialize
	IMPLICIT NONE

	CONTAINS
	! subroutine allocateMatrix	Alloziiert die Teilmatrix
	! subroutine initLineCount	Initialisiert Orientierung der Teilmatrix
	! subroutine initializeMatrix	Initalbelegung der Matrix 


	! Initialisiert lineCounts = Anzahl Zeilen die der Matrix yugeordnet sind
	! und offset = Versatz der Matrix gegen Gesamtmatrix
	subroutine initLineCount(lineCount, offset, matrixSize, numproc, rank)
		integer, intent(inout) :: linecount ! Anzahl der Zeilen, die der jeweiligen Matrix zugeordnet sind
		integer, intent(inout) :: offset ! Versatz der Matrix gegen Gesamtmatrix
		integer, intent(in) :: matrixSize ! Kantenlńge der Matrix
		integer, intent(in) :: numproc ! Anzahl der beteiligten Prozesse
		integer, intent(in) :: rank ! Rang des Prozessses
		integer, dimension(:), allocatable :: allLineCounts
		integer :: linesPerChunk, linesLastChunk
		integer :: i

		allocate(allLineCounts(numproc))
		
		! lineCounts
		linesPerChunk = matrixSize/numproc
		linesLastChunk = matrixSize - (linesPerChunk * (numproc - 1))
		do i=1,numproc-1
			allLineCounts(i) = linesPerChunk
		enddo
		allLineCounts(numproc) = linesLastChunk
		lineCount = allLineCounts(rank + 1)
		
		! Offset
		offset = 0
		do i = 1, rank
			offset = offset + allLineCounts(i)
		end do
		
		deallocate(allLineCounts)
		
	end subroutine initLineCount
	
	! Alloziiert Teilmatrix + Halolines 
	subroutine allocateMatrix(matrix, rank, numproc, lineCount, matrixSize)
		double precision, dimension(:,:), allocatable, intent(inout) :: matrix		
		integer, intent(in) :: matrixSize ! Kantenlńge der Matrix
		integer, intent(in) :: rank ! Rang des Prozessses
		integer, intent(in) :: numproc ! Anzahl der Prozesse
		integer, intent(in) :: linecount ! Anzahl der Zeilen, die der jeweiligen Matrix zugeordnet sind
		integer :: master = 0, last
		last = numproc - 1
		
		! master und last je eine Haloline
		if ((rank ==  master) .or. (rank == last)) then
			allocate(matrix((lineCount +1), matrixSize))
		else
		! Der Rest zwei.
			allocate(matrix((lineCount +2), matrixSize))
		endif
	end subroutine allocateMatrix
		
		
	! Belegt Ränder mit Startwerten
	subroutine initializeMatrix(matrix, rank, numproc, lineCount, offset)
		double precision, dimension(:,:), allocatable, intent(inout) :: matrix		
		integer, intent(in) :: rank ! Rang des Prozessses
		integer, intent(in) :: numproc ! Anzahl der Prozesse
		integer, intent(in) :: lineCount ! Anzahl der Zeilen, die der jeweiligen Matrix zugeordnet sind
		integer, intent(in) :: offset ! Versatz der Matrix gegener Gesamtmatrix
		double precision :: smallestStep ! Schrittgrösse für initiale Belegung an den Rändern
		integer :: master = 0, last ! Besondere Prozess IDs
		integer :: i, j ! Schleifenvariable

		!Belegen mit Nullen. (lieber alles. #YOLO)
		do i = lbound(matrix, 1), ubound(matrix, 1)
			do j = lbound(matrix, 2), ubound(matrix, 2)
				matrix(i,j) = 0
			end do
		end do
	
		! Belegung der Ränder
		!	Schema, grob:
		!	1	0.5	0
		!	0.5	0	0.5
		!	0	0.5	1
		!
		smallestStep = 1.0/(ubound(matrix, 2) - 1) !Schrittweite an den Rändern
		! links und rechts		
		do i = 2, lineCount + 1
			matrix(i, 1) = 1 - ((offset + i - 2) * smallestStep) !links
			matrix(i, ubound(matrix, 2)) = (offset + i - 2) * smallestStep !rechts
		end do	
		
		!oberer Rand
		if (rank == master) then
			do i = 1, ubound(matrix, 2)
				matrix(1, i) = 1 - ((i - 1) * smallestStep)
			end do	
		endif
		! unterer Rand
		if (rank == (numproc - 1)) then
			do i = 1, ubound(matrix, 2)
				matrix((lineCount + 1), i) = (i - 1) * smallestStep
			end do
		endif

	end subroutine initializeMatrix		
	
END MODULE initialize
