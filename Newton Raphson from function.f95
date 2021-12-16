program Newton_Raphson
integer a
double precision x1,x2,f,df
print*,'Give the initial value'
read*,x1
do a=1,7
	x2=x1-(f(x1)/df(x1))
	print*,x2
	x1=x2	
	
end do
end


double precision function f(x)
double precision x
f=x-cos(x)
end function


double precision function df(x) !df is differentation of f
double precision x
df=1+sin(x)
end function
