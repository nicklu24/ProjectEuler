program PE12
implicit none
real :: start, finish


call cpu_time(start)
call sieve2()
call cpu_time(finish)

print '("Elapsed: ", F15.6, " seconds")', finish - start


contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
 
!We know that a prime p will only have two divisors (1 and p), and we know that
!we only need to search until at most sqrt(n).  If we get any divisors from 2 to
!the limiting value, we know that the number is not prime and can end the
!search.
logical function isPrime(n) result (primeness)
implicit none
   integer*8, intent(in) :: n
   integer*8 :: lim 

        !Default result
        primeness = .true.
        
        
        do lim=2, Floor(sqrt(real(n)))
                if (mod(n,lim)==0) then
                      primeness=.false.
                      exit
                end if 
        end do 
   

end function


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
subroutine sieve2()
implicit none
        integer*8, dimension(2000000):: primes
        integer*8 :: i, j


        !First Populate the array
        do i= 2, size(primes)
                primes(i)=i

        end do 

        !Lets begin by removing(setting to zero) some nonprimes
        !We hit numbers multiple times, should look for way to escape this and
        !remove zeros from array
        !Also can look into using a logic array of size n, and looking at the
        !index position as the prime

        !remove all the evens
        do i=4, size(primes), 2
                primes(i)=0
        end do
        !remove multiples of 3
        do i=6, size(primes), 3
                primes(i)=0
        end do

        !remove multiples of 5
        do i=10, size(primes), 5
                primes(i)=0
        end do

        !remove multiples of 7
        do i=14, size(primes), 7
                primes(i)=0
        end do

        !remove multiples of 11
        do i=22, size(primes), 11
                primes(i)=0
        end do

        do i=13, size(primes)
                if (primes(i) .ne. 0) then
                    if (isPrime(primes(i)) ) then
                        do j=2*i, size(primes), i
                                primes(j)=0
                        end do
                    end if
                end if 
        end do
        
        
        write(*,*) sum(primes)
end subroutine



        





end program



