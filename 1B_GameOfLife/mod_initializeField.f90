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
		logical, dimension(:,:), pointer, intent(inout) :: matrix

		!Zuweisen der Matrix
		allocate(matrix(1:30,1:20))
		!Belegen mit "false"
		matrix = .false.
	end subroutine

	! Setzt die drei Figuren "blinker", "toad" und "beacon" auf das Spielfeld
	subroutine createFigures(matrix)
		! Spielfeld
		logical, dimension(:,:), pointer, intent(inout) :: matrix
		! Figuren
		logical, dimension(:,:), pointer :: blinker, toad, beacon
		! Koordinaten an denen die erste Figur eingesetzt wird. 
		! Alle anderen folgen in festgesetztem Abstand.
		integer :: positionX=4, positionY=3

		! Zuweisen und Erstellen der Figur blinker
		allocate(blinker(1:3,1:1))
		blinker = .true.

		! Zuweisen und Erstellen der Figur toad
		allocate(toad(1:4,1:2))
		toad = .true.
		toad(1,1) = .false.
		toad(4,2) = .false.

		! Zuweisen und Erstellen der Figur beacon
		allocate(beacon(1:6,1:6))
		beacon = .false.
		beacon(2:3,2) = .true.
		beacon(2,3) = .true.
		beacon(5,4) = .true.
		beacon(4:5,5) = .true.

		!Setzt Figuren auf die Matrix
		matrix(positionX:,positionY:) = blinker
		matrix(positionX+10:,positionY+5:) = toad
		matrix(positionX+15:,positionY+10:) = beacon
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
		call portable_sleep(0.3)
		deallocate(characterMatrix)
	end subroutine
	
end module

