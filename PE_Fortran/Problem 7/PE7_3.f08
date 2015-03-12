program PE7
implicit none
real :: start, finish


call cpu_time(start)
call sieve4()
call cpu_time(finish)

print '("Elapsed: ", F15.6, " seconds")', finish - start


contains


subroutine sieve4()
implicit none

        logical*1, dimension(250000) :: primes=.true.
        integer*8 :: i, j, x
        integer :: n 



        !sieve2 implemented with logic array
        do i=4, size(primes), 2
                primes(i)=.false.
        end do
        do i=6, size(primes), 3
                primes(i)=.false.
        end do
        do i=10, size(primes), 5
                primes(i)=.false.
        end do
        do i=14, size(primes), 7
                primes(i)=.false.
        end do
        do i=22, size(primes), 11
                primes(i)=.false.
        end do


        !We've eliminated nonprime indexes, if we do this consecutively we
        !eliminate all nonprimes and never need to isprime(n) them
        do i=13, int(sqrt(real(size(primes))))+5
                if (primes(i)) then
                        do j=2*i, size(primes), i
                                primes(j)=.false.
                        end do
                end if 
        end do
        
        n=0
        j=1
        do while (n<10002)
                
                if (primes(j)) then
                        n=n+1
                end if              
                j=j+1
        end do        
        write(*,*) (j-1)



end subroutine

        





end program


