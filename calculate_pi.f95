!random number generetor
Program random_number_generetor
implicit none
integer i,n
real x,y,pi,counting
print*,"Give number"
read*,n
counting=0
do i=1,n
call random_number(x)
call random_number(y)
!print*,x,y
if (x**2+y**2 .le.1) counting=counting+1
enddo
!print*,counting
print*,"The value of pi is",(counting/i)*4
end
