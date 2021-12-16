program deriv
implicit none
real h,x,f,df,d2f
h=0.001
x=2.

df=(f(x+h)-f(x))/h
print*,df
!d2f=(f(x+2*h)-2.*f(x+h)+f(x))/(h**2.)
d2f=(f(x+h)+f(x-h)-2*f(x))/h**2
print*,d2f
end



real function f(x)
real x
f=x**3+2*x**2
end function
