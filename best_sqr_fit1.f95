program best_fit
implicit none
integer i
real sumx,sumy,sumxy,sumx2,m,x(14),y(14)
open(133,file='practical_ko.dat',action='read')
  sumx=0
	sumy=0
	sumxy=0
	sumx2=0
do i=1,14
read(133,*) x(i),y(i)
		sumx=sumx+x(i)
		sumy=sumy+y(i)
		sumxy=sumxy+x(i)*y(i)
		sumx2=sumx2+x(i)**2.
end do
m=(14*sumxy-sumx*sumy)/(14*sumx2-(sumx)**2)
print*,m

end program
