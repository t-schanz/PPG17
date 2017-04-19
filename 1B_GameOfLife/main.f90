program main
	use mod_initializeField
	use mod_lifecycle
	implicit none

	! Spielfeld
	logical, dimension(:,:), pointer :: world
	! Lenkt Ausgabe auf Konsole
	integer :: outputUnit = 6
	! Schleifenvariable
	integer :: i

	! Umstellen des Zeichensatzes auf UTF-8 für stdout (nötig für printTwoDLogical)
	open(outputUnit, encoding='UTF-8')  

	! Subroutine in mod_initializeField inizilisiert Matrix world
	! gibt Matrix zurück
	call createField(world)
	! Subroutine in mod_initializeField belegt Matrix world mit figuren
	! gibt Matrix zurück
	call createFigures(world)

	! Subroutine in mod_initializeField gibt world auf Konsole aus
	call printTwoDLogical(outputUnit, world) 

	do i = 1,10
		! Subroutine in mod_lifecycle "Entwickelt leben" in Matrix world
		! gibt Matrix um ein lebenschritt weiter zurück
		call developeLife(world)
		! Subroutine in mod_initializeField gibt world auf Konsole aus
		call printTwoDLogical(outputUnit, world)
	end do

	call portable_sleep(0.3)
	deallocate(world)

end program
