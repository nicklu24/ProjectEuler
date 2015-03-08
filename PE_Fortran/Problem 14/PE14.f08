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
integer*8 :: counter=0, num, stager, bigcount=0, bignum=0

!We need to check all numbers below 1,000,000 to answer the question
do num = 1, 1000000
        !These are the dummies we'll operate on
        stager=num
        counter = 0
        !We need to get stager to 1
        do while (stager > 1)
                !If the number is even we halve it(Don't forget to increment
                !counter)
                if (mod(stager,2)==0) then
                        stager = (stager / 2)
                        counter = counter +1
                !Only other option is odd and we do 3n+1
                else
                        stager = (3*stager + 1)
                        counter = counter +1
                end if
        end do
        !If this is the longest chain we've seen we need to store it
        if (counter > bigcount) then
                bignum=num
                bigcount = counter
        end if
end do


write (*,*) bignum, " has the longest chain of: ", bigcount
                

end subroutine



!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!


end program







