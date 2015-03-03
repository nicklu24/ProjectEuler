program PE6
implicit none

real :: start, finish
call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F20.19, " second")', finish - start

contains

        subroutine ans()
        implicit none
        integer(kind=8) :: sigma=0, sigsquare=0
        integer :: i


                do i=1,100
                        sigma = sigma + i
                        sigsquare = sigsquare + i**2 
                end do

                print *, sigma**2 - sigsquare


         end subroutine

end program
