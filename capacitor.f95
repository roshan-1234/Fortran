!problem of capacitor
!Laplace equation 1-d
!V=5,x=5
program capacitor
implicit none
integer i
real*8 h
real*8 x(10),v(10)
!allocate (x(0:10,v(0:10)))
x(1)=0
v(1)=0
h=0.5
do i=1,10
!h=i/2.d0
x(i)=x(i)+h
v(i)=x(i)
print*,x(i),v(i)
h=h+0.5

end do
end

