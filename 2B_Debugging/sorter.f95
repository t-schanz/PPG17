module sorter
	!Debugging: Verhindert, dass "temp = array(cutPoint)" in Zeile 59 vom Compiler übersehen wird
	implicit none

	
contains
	! The most simple sorting algorithm with a bad performance.
	subroutine bubbleSort(array)
		real, dimension(:), intent(inout) :: array
		!DB Variablen initialisiert (tut das not?)
		integer :: i = 0, j = 0
		real :: temp
		!Debugging: Hier muss zum untersten Index j 1 addiert werden, da auf j-1 zugegriffen wird 
		do i = lbound(array, 1)+1, ubound(array, 1)
			do j = lbound(array, 1), ubound(array, 1)
				if(array(j - 1) > array(j)) then	! Are these two elements out of order? If so, swap them.
					temp = array(j - 1)
					array(j - 1) = array(j)
					array(j) = temp
				end if
			end do
		end do
	end subroutine

	! Do a simple insertion sort. Has quadratic complexity, but the inner loop is short.
	subroutine insertionSort(array)
		real, dimension(:), intent(inout) :: array
		integer :: i, j
		real :: temp

		do i = lbound(array, 1), ubound(array, 1)
			temp = array(i)
			!Debugging Lässt die Schleife bis 0 laufen, einen Schritt weiter.
			do j = i - 1, 0, -1
				if(array(j) <= temp) then
					array(j + 1) = temp
					exit
				!Debugging: Weitere Abfrage, die ausschließt, dass Element an die erste Stelle gehört
				else if (j /= 0) then
					array(j + 1) = array(j)
				!Debugging: Einordnung an erster Stelle
				else 
					array(j + 1) = temp
				end if
			end do
		end do
	end subroutine

	! A sorting algorithm designed for slowness...
	recursive subroutine slowsort(array)
		real, dimension(:), intent(inout) :: array
		integer :: elementCount, cutPoint
		real :: temp
		! Do we have to capitulate in face of the simplicity of sorting one element?
		elementCount = ubound(array, 1) - lbound(array, 1) + 1
		if (elementCount < 2) then
			return
		end if
		! Ok, we can do something. Lets multiply the problem.
		! The easiest way to compute the maximum is to split the array in two parts, sort them, look up their maxima, and swap the maximum if neccessary.
		!Debugging: Typumwandlung benötigt, damit das Programm sauber läuft
		cutPoint = int(real(elementCount)/2)
		call slowsort(array(1 : cutPoint))
		call slowsort(array(cutPoint + 1 : elementCount))
		if (array(cutPoint) > array(elementCount)) then
			!Debugging: Sollte natürlich nicht tenp, sondern temp heißen.
			temp = array(cutPoint)
			array(cutPoint) = array(elementCount)
			array(elementCount) = temp
		end if
		! So, we have got the maximum correct now. Sort the rest of the array...
		call slowsort(array(1 : elementCount - 1))
	end subroutine

	! Do a simple bucket sort. Has nlogn complexity, but with a rather long inner loop.
	recursive subroutine bucketSort(array, minimum, maximum)
		real, dimension(:), intent(inout) :: array
		real, intent(in) :: minimum, maximum	! Should be the min/max of the values to be sorted.
		real :: buffer(lbound(array, 1):ubound(array, 1))
		integer, parameter :: bucketCount = 10
		integer :: bucketSizes(0:bucketCount), bucketIndices(0:bucketCount), i, curBucket, curSize, nextBucketStart
			
		! Determine the size of each bucket.
		do i = lbound(array, 1), ubound(array, 1)
			curBucket = int(bucketCount*(array(i) - minimum)/(maximum - minimum))
			bucketSizes(curBucket) = bucketSizes(curBucket) + 1
		end do
		
		! Determine the start of each bucket.
		bucketIndices(0) = 1 
		do curBucket = 1, bucketCount
			bucketIndices(curBucket) = bucketIndices(curBucket - 1) + bucketSizes(curBucket - 1)
		end do	

		! Sort the values into the buckets.
		do i = lbound(array, 1), ubound(array, 1)
			curBucket = int(bucketCount*(array(i) - minimum)/(maximum - minimum))
			buffer(bucketIndices(curBucket)) = array(i)
			bucketIndices(curBucket) = bucketIndices(curBucket) + 1
		end do
		
		! Sort the buckets, bucketIndices now point past the end of the bucket.
		do curBucket = 0, bucketCount
			curSize = bucketSizes(curBucket)
			nextBucketStart = bucketIndices(curBucket)
			!Debugging: vorher (curSize > 0), Anpassung der minimalen Eimergröße verhindert endlose Rekursionen
			if(curSize > 1) then 
				call bucketSort(buffer(nextBucketStart - curSize : nextBucketStart - 1), &
				&               minimum + curBucket*(maximum - minimum)/bucketCount, &
				&               minimum + (curBucket + 1)*(maximum - minimum)/bucketCount)
			end if
		end do
		! Copy back.
		array = buffer
	end subroutine
	
	subroutine startBucketSort(array)
		real, intent(inout) :: array(:)
		call bucketSort(array, minval(array), maxval(array))
	end subroutine
end module

