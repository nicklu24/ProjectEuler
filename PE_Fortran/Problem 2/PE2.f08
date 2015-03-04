program PE2
implicit none


real*8 :: start, finish, time


call cpu_time(start)

call ans()

call cpu_time(finish)
time = finish - start
write (*,*) finish - start

Contains
        
        
        subroutine ans()
                integer*8 :: i, j, fib, sigma=2 
                !sigma=2 because we skip it in the fib sequence                
                i = 1          !first num of sequence
                j = 2          !second num

                do while (i < 4000000)
                        fib = i + j           !place holder
                        i = j                 !sets i to the next num
                        j = fib               !set j to the next num
                        if (mod(fib,2)==0) sigma = sigma + fib !sums the evens
                end do


                write (*,*) sigma


        end subroutine



end program

