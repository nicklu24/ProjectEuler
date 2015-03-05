program PE7
implicit none
real :: start, finish

call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F15.0, " seconds")', finish - start

contains

        
        Logical function isPrime(n) result(prime)
        implicit none
                integer, intent(in) :: n
                integer :: l
               

                prime = .true.
                if (n==1) then 
                        prime = .false.
                end if
                do l=2 , (n-1)
 
                        if (mod(n, l)==0) then
                                prime = .false.
                                exit
                        end if
                end do

         end function

        subroutine ans()
        implicit none
                integer*8 :: sigma = 0
                integer :: n = 1
        
                do n=1,2000000,2
                        if (mod(n,3)==0 .or. mod(n,5)==0 .or. & 
                        mod(n,7)==0 .or. mod(n,11)==0) then
                                cycle
                        end if
                        if (isprime(n)) then
                                sigma = sigma + n
                                
                        end if
                end do

                !Add back the primes we skipped
                write(*,*) (sigma + 2 + 3 + 5 + 7 + 11)           


        end subroutine

end program





