! program for bisection method
program bisection 
	real*8 a,b,c,f,error
	integer i
	print*,'Give the value of a and b='
	read*, a,b
	
	do
		c=(a+b)/2.
		
		if (f(a)*f(c).le.0)then
		b=c
		else
		a=c
		endif
		
		error=abs(a-b)/c
		if (error.le.1.E-10)exit
		
	enddo
print*,'The root is =',c
end

real*8 function f(x)
real*8  x
f=x**2-5*x+1
end
