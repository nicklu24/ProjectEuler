program PE14
implicit none
real :: start, finish


call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F15.6, " seconds")', finish - start

contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
subroutine ans()
implicit none
integer, dimension(400) :: num=0
integer :: power=0, pos=0

!intialize first position to 1.  We'll multiply by 2 1000 times
num(1)=1
Do power = 1, 1000
       num = 2*num
       !We need to carry ones
       Do pos = 400, 2, -1
                if (num(pos-1)>9) then 
                        !starting from the last digit of the number, we'll look
                        !to its left.  If that number is 10 or more, take ten
                        !away from it and add 1 to its neighbor to the right.
                        !We stop at two because during the process we take care
                        !of the element to the left.  That is at pos=2 we look
                        !at num(1) and fix it
                        num(pos) = num(pos) + 1
                        num(pos-1) = num(pos-1) - 10 
                        
                end if        
       end do
end do

write(*,*) sum(num)
 

end subroutine



!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!


end program








