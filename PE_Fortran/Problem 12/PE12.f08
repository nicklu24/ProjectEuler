program PE12
implicit none
real :: start, finish


call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F15.6, " seconds")', finish - start


contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!Function to count the number of divisors
!We will use the fact that the divisor pairs are mirrored around sqrt(n)
!That is d1, d2, d3, ...sqrt(n)....n  d1*n=n, d2*d(n-2)=n etc.
!Further note that sqrt(n) may or may not be included in the divisors depending
!on whether or not it is an integer

integer function NumDivisors(n) result (num)
implicit none
   integer*8, intent(in) :: n
   integer*8 :: lim, counter !counter needed because num is intent(in)

        
        counter = 0 !if we dont do this here it will wont get reset when
                    !function is run multiple times
        !We can stop the search when we get to sqrt(n)
        !Floor trucates the real
        do lim=1, Floor(sqrt(real(n)))
                if (mod(n,lim)==0) then
                        counter = counter + 1
                end if 
        end do 

        !As we stopped at sqrt(n) we don't have the full list. We need to
        !determine the remaining number of divisors.  If the the sqrt is an
        !integer it is a quasi double divisor(it is multiplied by itself to get
        !n, so it isnt a part of a pair) so we take twice the count minus one
        !appearance of n.  If n is not an integer then we just double the count.
        !The follow if block does that.
        if (Floor(sqrt(real(n)))**2 == n) then
                num = (2*counter - 1)
        else
                num = 2*counter
        end if 

end function
!I attempted to make the upper limit for input n greater but was limited by data
!type errors. Namely errors abounded when sqrt(n) was too large to fit into an
!integer type, thus not working in its limit capacity in the do 1,sqr.  This
!could be worked around possibly by using a do while true.  Inside this do while
!one could operate on a real*8 value and have lim=lim-1 every loop and an if
!lim<0 exit type statement.  The next problem would be in the mod(n,lim) because
!this wouldn't work if lim was real...... and determining the final count would
!need agumenting
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        subroutine ans()
        implicit none
        integer*8, dimension(100000):: trianglenums
        integer :: n
        
        trianglenums(1) = 1
        n=2
        !Treating trianglenums as an array was an experiment/practice to see how
        !quickly it ran.  Same with do while .true. it could be done as a do while
        !(numdivisors < 500)
        do while (.true.) 
           trianglenums(n) =  trianglenums(n-1) + n
           if (NumDivisors(trianglenums(n)) .ge. 500) then
                   write(*,*) trianglenums(n)
                   exit
           end if 
           n=n+1
        end do    


        end subroutine




end program
