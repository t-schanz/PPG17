module mod_initializeField
	use mo_utilities
	implicit none

	! Einbinden des Zeichensatzes UTF-32
	! (notwendig für die Methode printTwoDLogical)
	private utf32
	integer, parameter :: utf32=selected_char_kind('ISO_10646')

contains
	! subroutine createField(matrix)
	! subroutine createFigures(matrix)
	! subroutine printTwoDLogical(outputUnit, matrix)
	

	! Erstellt Spielfeld mit Dimensionen 30x20
	subroutine createField(matrix)
		!Spielfeld
		logical, allocatable, dimension(:,:), intent(inout) :: matrix

		!Zuweisen der Matrix
		allocate(matrix(1:30,1:20))
		!Belegen mit "false"
		matrix = .false.
	end subroutine

	! Setzt die zwei Figuren, "Glider" und "lwss" (lightweight 
	! spaceship) auf das Spielfeld
	subroutine createFigures(matrix)
		! Spielfeld
		logical, allocatable,  dimension(:,:), intent(inout) :: matrix
		! Figuren
		logical, allocatable, dimension(:,:):: glider, lwss
		! Koordinaten an denen die erste Figur eingesetzt wird. 
		! Alle anderen folgen in festgesetztem Abstand.
		integer :: positionX=3, positionY=2

		! Zuweisen und Erstellen der Figur glider
		allocate(glider(1:3,1:3))
		glider = .true.
		glider(2,1) = .false.
		glider(1,2:3) = .false.
		glider(3,3) = .false.

		! Zuweisen und Erstellen der Figur lwss (lightweight spaceship)
		allocate(lwss(1:5,1:4))
		lwss = .false.
		lwss(1,1) = .true.
		lwss(1,3) = .true.
		lwss(2:5,4) = .true.
		lwss(4,1) = .true.
		lwss(5,2:3) = .true.


		!Setzt Figuren auf die Matrix
		matrix(positionX:,positionY:) = lwss
		matrix(positionX+5:,positionY+10:) = glider

	end subroutine 

	
	! Ausgabe auf die Konsole
	! (Übernommen aus dem Programm Glider)
	subroutine printTwoDLogical(outputUnit, matrix)
		integer, intent(in) :: outputUnit
		logical, dimension(:,:), intent(in) :: matrix
		integer :: width, height
		character(kind=utf32, len=1), dimension(:,:), allocatable :: characterMatrix
		! z'1b' hexadezimale Zahl, dezimaler Wert ist 27 
		! char(int(z'00B7'), utf32) ist das Zeichen mit der hexad. Nummer 00B7 
		! des Zeichensatzes 'ISO_10646'
		! 00B7 = middle dot, 2588 = full block
		character(kind=utf32, len=1) :: blockChar = char(int(z'2588'), utf32), emptyChar = char(int(z'00B7'), utf32)
		character(len=22) :: formatString
		
		!Create the character matrix.
		width = ubound(matrix, 1)
		height = ubound(matrix, 2)
		allocate(characterMatrix(1:width, 1:height))
		where (matrix)
			characterMatrix = blockChar
		else where
			characterMatrix = emptyChar
		end where

		!Write it out.
		formatString = '(' // intToStr(width) // 'a)'
		write(outputUnit, formatString) characterMatrix
		formatString = '(' // intToStr(width) // '("="))'
		write(outputUnit, formatString)
		
		!Waste some time.
		call portable_sleep(0.1)
		deallocate(characterMatrix)
	end subroutine
	
end module

