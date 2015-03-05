program PE7
implicit none
real :: start, finish


call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F15.6, " seconds")', finish - start

contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!First we have a function to comput the max product from multiplying four times to the right
!Note that right multiplicative max is the left max b/c its commutative
integer*8 function RightMax(table) result(rm)
        implicit none
        integer, intent(in), dimension(20,20) :: table
        integer :: row, col, x=1
        integer*8 :: sigma=1
        rm=1
                
        
           do x=1,17    !X tracks the x position in the row
                do row = 1, 17  !Row tracking
                     !Start at the first x position and multiply the four
                     !elements 
                        do col = x, x+3
                                sigma = sigma * table(row, col)
                        end do
                        !if this is the highest returned value store it
                        if (sigma > rm) rm=sigma
                        !RESETS sigma to 1
                        sigma = 1
                !Do the next row starting at same x   
                end do 
           !Start with the next x position
           end do    

        end function
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!Function to compute the max of up/down multiplication
!transpose the matrix the do right max
!this has the effect of making down right

integer*8 function DownMax(table) result (dm)
        implicit none
        integer, intent(in), dimension(20,20) :: table
        integer, dimension(20,20) :: trans

        trans=transpose(table)

        dm = RightMax(trans)

 
end function


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!Function to compute the normal diagonal multiplication with four elements
!(normal as in down and to the right)

Integer*8 function DiagMax(table) result (DiaM)
        implicit none
        integer :: row, col, x, y, counter
        integer, intent(in), dimension(20,20) :: table
        integer*8 :: sigma =1
        DiaM = 1

            Do x=1,17
               Do row = 1,17
                   col = x
                   sigma =1     !resets sigma each time
                   !implement a counter to do (r+0,c+0) *(r+1,c+1)...(r+3,c+3)
                   Do counter= 0, 3     
                       sigma = sigma * table((row+counter), (col+counter))
                   end do
                   if (sigma > DiaM) DiaM= sigma
               end do
            end do






end function

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!Finds the max on the down left diagonal
Integer*8 function DiagLeft(table) result (DiaL)
        implicit none
        integer :: row, col, x, y, counter
        integer, intent(in), dimension(20,20) :: table
        integer*8 :: sigma =1
        DiaL = 1

            Do x=20,4,-1
               Do row = 1,17
                   col = x
                   sigma =1     !resets sigma each time
                   !implement a counter to do (r+0,c-0) *(r+1,c-1)...(r+3,c-3)
                   !this goes down in rows and left in columns
                   Do counter= 0, 3    
                       sigma = sigma * table((row+counter), (col-counter))
                   end do
                   if (sigma > DiaL) DiaL= sigma
               end do
            end do






end function

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        subroutine ans()
        implicit none
        integer, dimension(20, 20) :: d
        d = reshape( (/ 08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, &
                12, 50, 77, 91, 08, & 
        49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00, &
        81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65, &
        52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91, &
        22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80, &
        24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50, &
        32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70, &
        67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21, &
        24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72, &
        21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95, &
        78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92, &
        16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57, &
        86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58, &
        19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40, &
        04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66, &
        88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69, &
        04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36, &
        20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16, &
        20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54, &
        01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48 /) &
        , shape(d))

        write(*,*) "Right/Left Max:", RightMax(d)
        write(*,*)"Up/down Max:", DownMax(d)
        write(*,*) "Forwards Diagonal Max:", DiagMax(d)
        write(*,*) "Backwards Diagonal Max:", DiagLeft(d)
        write(*,*) "Global Max:" ,&
        max(RightMax(d),DownMax(d),DiagMax(d),DiagLeft(d))

        end subroutine

end program






