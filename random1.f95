!random number generetor
Program random_number_generetor
implicit none
integer i
real x,a,b,c
print*,"Lower and upper value in which random number to print"
read*,a,b
do i=1,10
call random_number(x)
c=a+(b-a)*x
!print*,2*x
!print*,c
enddo
real function f(x)

end
