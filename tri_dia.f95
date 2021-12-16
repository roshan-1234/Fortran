program tridigonal
implicit none
integer i,j,k
real b(5),a(5),c(5),x(5),r(5),beta(5),rho(5)
data b/2,2,2,2,2/
data a/0,-1,-1,-1,-1/
data c/-1,-1,-1,-1,0/
data r/0,1,2,3,4/
beta(1)=b(1)
rho(1)=r(1)
do j=2,5

	beta(j)=b(j)-(a(j)/beta(j-1))*c(j-1)
	!print*,beta(j)
end do

do j=2,5
	rho(j)=r(j)-(a(j)/beta(j-1))*rho(j-1)

	!print*,rho(j)
	
end do

x(5)=rho(5)/beta(5)
!print*,x(5)

do j=1,4
	x(5-j)=(rho(5-j)-(c(5-j)*x(6-j))/beta(5-j))
	print*,x(5-j)


end do

end
