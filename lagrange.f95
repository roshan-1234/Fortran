program interpolation

implicit none
integer::i,j
real  (kind=8)xx,yy,prod,summ
real x(5),y(5)
data x/1,2,3,4,5/
data y/1,4,9,16,25/
print*,'give  the value wich want to find'
read*,xx

summ=0
do i=1,5
	
	prod=1
	do j=1,5
	if (i .ne.j) then
		prod=prod*( xx-x(j) ) / ( x(j)-x(i) )
	end if
	end do
 summ=summ+prod*y(i)
end do
print*,summ
end program
