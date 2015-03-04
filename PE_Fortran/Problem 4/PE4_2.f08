program PE4_2
implicit none
real :: start, finish

call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F9.6, " second")', finish - start

contains

        subroutine ans()
        implicit none
        integer:: i, j, k, ones, tens, hunds, thou, tenthou, sigma = 0
        
        do i= 999, 100, -1
                do j= 999, 100, -1

                        k= i*j !what realm is this?? the quaternions?

                        !stop if k < sigma
                        If (k > sigma) then
                                continue
                        else
                                exit
                        end if

                        !First check if the first and last digit are the same
                        !Grab the ones with modulo 10
                        !grab hundthou by int div by 100,000
                        ones = mod(k,10)                        
                        if (k/100000==ones) then
                                !check next digits
                                !get tens by first getting last two, then integer
                                !division by 10 
                                !get tenthou by subtracting off first digit and
                                !then int div by 10,000 
                                tens = mod(k,100)/10
                                if ((k-100000*ones)/10000==tens) then
                                        !Do it again
                                        hunds = mod(k,1000)/100
                                        if ((k-ones*100000-tens*10000)/1000==hunds) then
                                                sigma= k
                                        end if
                                end if
                        end if
                 end do
         end do 

         write (*,*) sigma



        end subroutine

end program



