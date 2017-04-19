
module mod_lifecycle
	!
	implicit none
	private :: countNeighbours !(matrix)
	public :: developLife !(matrix, posX, posY, neighbors)
	
contains

	! Entwickelt das Spielfeld anhand der folgenden Regeln: (dabei steht lebend für "true" in der Matrix)
	! - Lebende Zellen mit weniger als zwei Nachbarn sterben.
	! - Lebende Zellen mit mehr als drei Nachbarn sterben.
	! - Lebende Zellen mit zwei oder drei Nachbarn überleben.
	! - Tote Zellen mit genau drei Nachbarn werden belebt.
	!
	subroutine developeLife(matrix)
		!
		! Spielfeld
		logical, dimension(:,:), intent(inout) :: matrix
		! Interne Kopie des Spielfelds
		logical, dimension(:,:), pointer :: matrixCopy
		! Dimensionen des Spielfelds
		integer :: width, height
		! Schleifenvariablen und Hilfsvariable
		integer :: i, j, neighbours

		width = ubound(matrix, 1)
		height = ubound(matrix, 2)
		allocate(matrixCopy(1:width,1:height))
		matrixCopy = matrix

		do i = 1,width
			do j = 1,height
				call countNeighbors(matrixCopy, i, j, neighbours)
				! Eigentlich Anwendung der Regeln aufs Spielfeld
				if (matrixCopy(i, j) .eqv. .true.) then
					matrix(i,j) = ((neighbours > 1) .and. (neighbours < 4))
				else
					matrix(i,j) = (neighbours == 3)
				end if

			end do 
		end do
	end subroutine


	! Zählt lebende Nachbarn einer Zelle
	!	matrix: Spielfeld
	!	posX, posY: Koordinaten der Zelle im Spielfeld
	!	neighbours: Anzahl der lebendigen Nachbarn einer Zelle
	!
	subroutine countNeighbors(matrix, posX, posY, neighbors)
		!
		! Spielfeld
		logical, dimension(:,:), intent(in) :: matrix
		! Koordinaten
		integer, intent(in) :: posX, posY
		! (Rückgabewert) Anzahl der lebendigen Nachbarn einer Zelle
		integer, intent(out) :: neighbors
		! Dimensionen des Spielfelds
		integer :: width, height
		! Grenzen innerhalb derer Nachbarn gezählt werden
		integer :: left, right, top, bottom
		! Schleifenvariablen
		integer :: i, j

		neighbors = 0
		width = ubound(matrix, 1)
		height = ubound(matrix, 2)
		! Initialisierung der Grenzen mit Abstand 1 zu den Koordinaten
		left = posX-1
		right = posX+1
		top = posY-1
		bottom = posY+1
		! Anpassung der Boundarys für Felder am Spielfeldrand
		if(posX == 1) left = posX
		if(posX == width) right = posX
		if(posY == 1) top = posY
		if(posY == height) bottom = posY

		! Zählen der lebenden Nachbarn in einem 3x3-Kasten um die Koordinate 
		do i = left,right
			do j = top,bottom
				if(matrix(i,j) .eqv. .true.) then       
					neighbors = neighbors + 1
				end if
			end do
		end do

		! Korrektur um den Wert der Zelle selbst
		if (matrix(posX, posY) .eqv. .true.) then
					neighbors = neighbors -1 
		end if

	end subroutine
end module
