program PE4
implicit none
real :: start, finish

call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F9.6, " second")', finish - start

contains

        subroutine ans()
        implicit none
        integer:: i, j, k, ones, tens, hunds, thou, tenthou, hundthou, sigma
        
        do i= 999, 100, -1
                do j= 999, 100, -1
                        k= i*j !what realm is this?? the quaternions?
                        !First check if the first and last digit are the same
                        !Grab the ones with modulo 10
                        ones = mod(k,10)
                        tens = mod(k,100)/10
                        hunds = mod(k,1000)/100
                        hundthou = k/100000
                        tenthou = (k - ones*100000)/10000
                        thou =(k - ones*100000 - tens*10000)/1000 

                         if (ones==hundthou .and. tens==tenthou&
                         .and. hunds==thou) then
                                if ( k> sigma) then
                                        sigma= k
                                        exit
                                end if 
                        end if       
!                        if (k/100000==ones) then
!                                !check next digits
 !                               !get tens by first getting last two, the integer
  !                              !division by 10  
   !                             tens = mod(k,100)/10
!                                !remove the first digit(multiply ones to correct
 !                               !digit and subtract) truncate
  !                              if ((k-100000*ones)/10000==tens) then
   !                                     !Do it again
    !                                    hunds = mod(k,1000)/100
     !                                   if ((k-ones*100000-tens*10000)/1000==hunds) then
      !                                          sigma= k
       !                                 end if
        !                        end if
         !               end if
                 end do
         end do 

         write (*,*) sigma



        end subroutine

end program


