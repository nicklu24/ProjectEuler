program PE5
implicit none
real :: start, finish

call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F9.6, " second")', finish - start

contains

        subroutine ans()
        implicit none
                integer :: inc = 0
                integer :: num =20
                integer :: i 
                
                Do while (inc < 20)
                        Do i =1,20
                                If (mod(num,i)==0) then
                                        inc = inc + 1
                                Else
                                        num = num + 20
                                        inc=0                                       
                                End if
                                 

                        End do
                 End do

                       
                 print *, num
                       






        end subroutine
end program
