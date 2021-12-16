!calculate first derivatice by approximation
program approximation

implicit none
real x(4)

real h,f,x1,x2,x3,x4,two_back,two_for,two_cen,three_for

data x/1.2,1.4,1.6,1.8/
h=0.2
!!two point backward
!print*,x(2),'and',x(1)
two_back=(f(x(2))-f(x(1)))/h
print*,'The value of derivatve using two point backward=',two_back
!!two point forward
!print*,x(3),x(2)
two_for=(f(x(3))-f(x(2)))/h
print*,'The value of derivative using two point forword=',two_for
!!two point central
!print*,x(3),x(1)
two_cen=(f(x(3))-f(x(1)))/(2*h)
print*,'the value of derivative using two point central=',two_cen
!!three point forward
!print*,x(2),x(3),x(4)
three_for=(-3*f(x(2))+4*f(x(3))-f(x(4)))/(2*h)
print*,'the value of derivative using the point forward=',three_for



end program


real function f(x)
	implicit none
	real x,exp
	f=sin(x/2)*exp(-x)
end function






