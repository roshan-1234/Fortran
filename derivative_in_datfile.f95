program deriv
implicit none
integer i,k
real(8) h,x,f,df,d2f,err1
print*,'give the value of h'
read*,h
print*,h
open (unit=120,file='derivative.dat')
x=2.
write(120,*) 'h','df','d2f'
do i=1,15
k=10
!h=h/k
df=(f(x+h)-f(x))/h
!d2f=(f(x+h)+f(x-h)-2*f(x))/h**2
err1=df-(3*x**2.+4*x)
h=h/k
write(120,*) h,df,d2f,err1
print*,h,err1
enddo
end
real(8) function f(x)
real(8) x
f=x**3+2*x**2
end function
