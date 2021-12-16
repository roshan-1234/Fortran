program sqrt_b
implicit none
integer i
real a, saqrt1
print*,'Give a number whose square root is to be determined'
read*,a
sqrt1=1
do i=1,10
sqrt1=0.5*(a/sqrt1+sqrt1)
enddo
print*,'the sqrt root is  1',sqrt1
end

