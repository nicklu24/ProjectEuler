program PE5
implicit none
real :: start, finish

call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F9.6, " second")', finish - start

contains

        subroutine ans()
        implicit none
                integer :: inc = 0
                integer :: num =20
                integer :: i 
                Do while ( mod(num,7) /= 0 .or. mod(num,11) /=0 .or. mod(num,12) /= 0 .or. mod(num,13) /=0  .or. &
                mod(num,14) /=0 .or. mod(num,15) /= 0 .or. mod(num,16) /=0  .or. mod(num,17) /=0 .or. mod(num,18) /= 0 .or. &
                mod(num,19) /=0 .or. mod(num,20) /=0)
                        num = num + 20
                end do 

                print *, num
        end subroutine

end program

