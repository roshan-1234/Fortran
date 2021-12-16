program file
implicit none
integer i
real x,y,z,a,b,c
open(110,file='hari.dat')
open(120,file='Ram.dat')
open(130,file='shyam.dat')
open(140,file='all.dat')
do i=1,9
	read(110,*) a,x
	read(120,*) b,y
	read(130,*) c,z
	write(140,*) a,x,y,z
end do
!write(140,*) x,y
end
