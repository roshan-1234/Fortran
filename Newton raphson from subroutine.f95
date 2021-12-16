!Newton Raphson method by using subroutine
program Newton_Raphson
implicit none
integer i
double precision x1,x2,d,z
print*,"give the value of x1"
read*,x1
call subroutine f(x1)
call subroutine df(x2)
do i=1,7
	x2=f(x1)-f(x1)/df(x1)
	print*,x2
	x1=x2
end do

end 

subroutine f(x)
	implicit none
	double precision x,z
	z=x-cos(x)
end subroutine

subroutine df(x)
	implicit none 
	double precision x,d
	d=1+sin(x)
end subroutine
