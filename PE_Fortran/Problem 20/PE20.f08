program PE20
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
integer*2, dimension(300) :: num=0
integer :: n=0, pos=0


!We'll make a factorial array of byte sized integers
!intialize first position to 1.  
num(1)=1
Do n = 1, 100
       num = n*num
       !We need to carry ones
       Do pos = 2, size(num), 1
                if (num(pos-1)>9) then 
                        !starting from the second digit of the number, we'll look
                        !to its left.  If that number is 10 or more, take its tens
                        !away from it and add to its neighbor to the right.
                        num(pos) = num(pos) + num(pos-1)/10
                        num(pos-1) = mod(num(pos-1),10)
                        
                end if        
       end do
end do

write(*,*) sum(real(num))

end subroutine



!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!


end program









