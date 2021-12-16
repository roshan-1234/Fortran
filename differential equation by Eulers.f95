!Euler method for solving diff eqn dy/dx with y(0)=0 of step taken=10
program euler
implicit none 
	integer i,n
	real(8) a,b,f,h
	real(8), allocatable::x(:),y(:)
	print*,"Give number of step for euler calculation"
	read*,n 
	allocate (x(0:n),y(0:n))
	x(0)=0
	y(0)=0
	a=0
	b=0.5
	h=(b-a)/n
	do i=0,n
			x(i+1)=x(i)+h
			y(i+1)=y(i)+h*f(x(i),y(i))
			print*,x(i),y(i)
			!deallocate x(i),y(i)
	end do
end

real(8) function f(x,y)
real(8) x,y
f=X**3+y**3
endfunction
		
