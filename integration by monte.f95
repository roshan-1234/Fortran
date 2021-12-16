!random number generetor
Program random_number_generetor
implicit none
integer i,n
real x,a,b,c,f,summ
summ=0
print*,"Lower and upper value in which random number to print"
read*,a,b
print*,'number of points'
read*,n
do i=1,n
call random_number(x)
c=a+(b-a)*x
summ=summ+f(c)
end do
print*,"Integration=",((b-a)*summ)/n
end



real function f(x)
real x
f=x**3.
end function
