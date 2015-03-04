program Combinations
implicit none


real*8 :: start, finish, time


call cpu_time(start)

call ans()

call cpu_time(finish)
time = finish - start
write (*,*) finish - start

Contains
        
        
        subroutine ans()
                integer :: i, sigma=0

                Do i= 1, 999
                        if (mod(i,3)==0 .or. mod(i,5)==0) then
                                sigma = sigma + i
                        end if
                end do 

                write(*,*) sigma
        end subroutine



end program
