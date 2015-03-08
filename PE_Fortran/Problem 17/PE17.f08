program PE14
implicit none
real :: start, finish


call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F15.6, " seconds")', finish - start

contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!Let's use a function to count the number of letters in a numbers name
!We first need to know how many letters are in each word
!one, two, six, ten-3
!three, seven, eight - 5
!four, five, nine - 4
!eleven, twelve -6
!thirteen, fourteen - 8 
!fifteen-7
!sixteen through nineteen are the bases concatenated with teen - base + 4
!20-39; 80-99 - 6 plus base
!forty-69 - 5 plus base
!seventy-79 - 7 plus base
!hundred - 7 plus base
!hundred and - 10 plus base
!thousand - 8 plus base


integer function NumOfLetters(num) result(length)
implicit none
integer, intent(in) :: num
integer :: tracker, dnum
!we have the input, a variable to track the length, and a dummy number that we
!can alter

tracker = 0

!Check if we have "hundred" in the number
if (num>= 100 .and. num < 1000) then
        tracker = tracker + 7
        !Check if we have an and, and what the rest of the number is
        if ( mod(num,100) /=0) then
                tracker = tracker + 3
                if (num/100==1 .or. num/100==2 .or. num/100==6) then
                                tracker = tracker + 3 
                        elseif (num/100==4 .or. num/100==5 .or. num/100==9) then 
                                tracker = tracker + 4
                        elseif (num/100==3 .or. num/100==7 .or. num/100==8) then
                                tracker = tracker + 5
                end if
        else
                dnum = (num/100)
                goto 888        
        end if 
        !Get rid of the the hundreds digit
        dnum = num - (num/100)*100
elseif (num==1000) then
        length = 11 !one thousand = 11
        goto 999
else
        dnum= num
end if 

!Need to find the word length of the rest of the number
if ((dnum >=20 .and. dnum<=39) .or. (dnum>=80 .and. dnum<=99) ) then
        tracker = tracker + 6 
        dnum = dnum - 10*(dnum/10)
elseif (dnum >=40 .and. dnum<=69) then
        tracker = tracker + 5
        dnum= dnum - 10*(dnum/10)
elseif (dnum >=70 .and. dnum<=79) then
        tracker= tracker + 7
        dnum = dnum -10*(dnum/10)
elseif (dnum==13 .or. dnum==14 .or. dnum==18 .or. dnum==19) then
        length = tracker + 8
        goto 999
elseif (dnum==17) then
        length = tracker + 9
        goto 999        
elseif (dnum==15 .or. dnum==16) then
        length = tracker + 7
        goto 999
elseif (dnum==11 .or. dnum==12) then
        length = tracker + 6
        goto 999
elseif (dnum ==10) then
        length = tracker + 3 
        goto 999
end if 

888 &
if (dnum==1 .or. dnum==2 .or. dnum==6) then
        length = tracker + 3 
elseif (dnum==4 .or. dnum==5 .or. dnum==9) then 
        length = tracker + 4
elseif (dnum==3 .or. dnum==7 .or. dnum==8) then
        length = tracker + 5
else if( dnum==0) then
        length = tracker
end if 




999  end function



!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
subroutine ans()
implicit none
integer :: sigma=0, n
integer:: numbers(1000)

do n=1, 1000
        numbers(n) = numofletters(n)
end do

do n=1,1000
        sigma = sigma + NumOfLetters(n)
end do


write (*,*) sigma
write(*,*) sum(numbers)
write(*,*) "Three hundred and forty-two" , numofletters(342)
write(*,*) "one hundred", numofletters(100)

 

end subroutine



!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!


end program









