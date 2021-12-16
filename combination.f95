!permutation and combination of two numbers
program subprogram_3
implicit none
integer i,j,k
real p,c !p is  permutation and c is combination
print*,'GIve two number whose permutation and combination is  to  be found'
read*, i,j
p=factorial(i)/factorial(i-j)
 c=p/factorial(j)
print*,'The permutation and combination are',p,c
end



integer function factorial(n)
integer i,n
factorial=1
do i=1,n
factorial=i*factorial
enddo
end function
