program PE7
implicit none
real :: start, finish

call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F9.6, " seconds")', finish - start

contains

        
        Logical function isPrime(n) result(prime)
        implicit none
                integer, intent(in) :: n
                integer :: l
               

                prime = .true.
                do l=2 , (n-1)
                        if (mod(n, l)==0) then
                                prime = .false.
                                exit
                        end if
                end do

         end function

        subroutine ans()
        implicit none
                integer :: sigma = 0, n = 1
        
                do while (sigma<10002)
                        if (isprime(n)) then
                                sigma = sigma + 1
                        end if
                        n = n + 1
                end do 
        
               
                write (*,*) (n-1)
                write (*,*) isPrime(4)

        end subroutine

end program




