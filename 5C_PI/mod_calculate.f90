module mod_calculate
    contains
 

        subroutine func(start,ende,pi, preci)
            double precision, intent(inout) :: pi
            real, intent(in) :: preci
            double precision :: x,f

                f = 4./1. 
                do x=start,ende,(1/preci)
                    !f= (f + ( 4./(1.+(x*x)) ) ) /2.
                
                    f = (4./(1.+(x*x)))
                    pi = (f+pi)
                enddo
            
                pi = pi / preci

            
        end subroutine func

end module 
