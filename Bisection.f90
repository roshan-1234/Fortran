!find out the root by using bisection method
program bisection_method
implicit none
double precision::l,r,m,f,error
!external f

print*, 'give the left and right value which lies in bounded value of root'
read*, l,r

if (f(l)*f(r)>0.) then
print*,'your l and r value doesnot lie in the root,please put again land r inside the root range'

stop
end if
do
m=(l+r)/2
if (f(l)*f(m)<=0.) then
r=m
else 
l=m
endif
error = abs (l-r)/m
if(error<=1.E-15)exit
enddo
write(*,*) 'root is ' ,m
end 

double precision function f(x)
double precision :: x
f= x**2-5*x+1
end function 
