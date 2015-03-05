program PE9
implicit none

real :: start, finish
call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F20.19, " second")', finish - start

contains

        subroutine ans()
        implicit none
        integer*8 :: a, b, aa=0, bb=0

        !limits on do loop from basic algebra, nums exceeding bounds 
        do a=1, 707
                do b=1,708
                        if(sqrt(real((a**2)+(b**2)))+a+b > 1000) cycle
                        if(sqrt(real((a**2)+(b**2)))+a+b==1000) then
                                aa=a
                                bb=b
                                exit
                        end if
                end do
        end do

        write(*,*) aa*bb*int(sqrt(real(aa**2+bb**2)))


         end subroutine

end program

