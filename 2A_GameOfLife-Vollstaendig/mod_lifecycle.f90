module mod_lifecycle
	implicit none
contains
	! subroutine developeLife(matrix)	
	! subroutine countNeighbors(matrix, posX, posY, neighbors)


	! Develops the given matrix by the following rules: (where alive means logical true)
	! - Any alive cell that is touching less than two alive neighbours dies.
	! - Any alive cell touching four or more alive neighbours dies.
	! - Any alive cell touching two or three alive neighbours does nothing.
	! - Any dead cell touching exactly three alive neighbours becomes alive.
	!
	subroutine developeLife(matrix)
		!
		! field
		logical, dimension(:,:), intent(inout) :: matrix
		! internal copy of the field
		logical, allocatable, dimension(:,:) :: matrixCopy
		! dimensions of the field
		integer :: width, height
		! loop counter, tempory storage
		integer :: i, j, neighbours

		width = ubound(matrix, 1)
		height = ubound(matrix, 2)
		! matrix Groesse zuweisen, auf jeder Seite 1 Feld groesser als
		! Eingangs-Matrix
		allocate(matrixCopy(0:width+1,0:height+1))

		! mit false belegen
		matrixCopy = .false.
		!Matrix auf matrixCopy schreiben
		matrixCopy(1:,1:) = matrix

		! Fuer zyklische Randbedingungen
		! Die vorletzte Reihe wird auf die gegenüberligende letzte Reihe
		! geschrieben (fuer jeweils die 4 Raender)
		matrixCopy(:,0) = matrixCopy(:,height)
		matrixCopy(:,height+1) = matrixCopy(:,1)
		matrixCopy(0,:) = matrixCopy(width,:)
		matrixCopy(width+1,:) = matrixCopy(1,:)


		do i = 1,width
			do j = 1,height
				call countNeighbors(matrixCopy, i, j, neighbours)
				if (matrixCopy(i, j) .eqv. .true.) then
					matrix(i,j) = ((neighbours > 1) .and. (neighbours < 4))
				else
					matrix(i,j) = (neighbours == 3)
				end if

			end do 
		end do
	end subroutine


	! Counts alive neighbours of one cell 
	!	matrix, posX, posY: matrix and coordinates of the cell in this matrix
	!	neighbours: alive neighbours of the cell
	!
	subroutine countNeighbors(matrix, posX, posY, neighbors)
		!
		! field
		logical, dimension(:,:), intent(in) :: matrix
		! coordinates
		integer, intent(in) :: posX, posY
		! return value: number of alive neighbors
		integer, intent(out) :: neighbors
		! loop counter
		integer :: i, j

		neighbors = 0

		! Abzaehlen der lebenden Zellen in 3x3 Box  
		! Beginnt bei posX, das entspricht der Matrix in developeLife 
		! der Zelle posX-1, da beim uebergeben der Matrix mit der 
		! Dimension (0:X,0:Y) eine Matrix mit der Dimension (1:x+1, 
		! 1:y+1) ensteht
		do i = posX,posX+2
			do j = posY,posY+2
				if(matrix(i,j).eqv. .true.) then       
					neighbors = neighbors + 1
				end if
			end do
		end do

		! Korrektur fuer lebende Zellen
		if (matrix(posX+1, posY+1) .eqv. .true.) then
					neighbors = neighbors -1 
		end if

		

	end subroutine
end module
