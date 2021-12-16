program laplace_em
implicit none
integer,parameter::P=31
logical,dimension(P,P)::isConductor
real(8),dimension(P,P)::V
real(8)::V1,V2,epsilon
integer::L
L=P
print*,'EnterV1,V2:'
read*,V1,V2
print*,'Enterepsilon:'
read*,epsilon
print*,'StartingLaplace:'
print*,'GridSize=',L
print*,'ConductorssetatV1=',V1,'V2=',V2
print*,'Relaxingwithaccuracyepsilon=',epsilon
call initialize_lattice(V,isConductor,L,V1,V2)
call laplace(V,isConductor,L,epsilon)
call print_results(V,L)
end program laplace_em

subroutine initialize_lattice(V,isConductor,L,V1,V2)
implicit none
integer::L
logical,dimension(L,L)::isConductor
real(8),dimension(L,L)::V
real(8)::V1,V2
integer::i,j
V=0.0D0
isConductor=.FALSE.
do i=1,L
	isConductor(1,i)=.TRUE.
	isConductor(i,1)=.TRUE.
	isConductor(L,i)=.TRUE.
	isConductor(i,L)=.TRUE.
enddo
do i=5,L-5
	V(L/3+1,i)=V1
	isConductor(L/3+1,i)=.TRUE.
	V(2*L/3+1,i)=V2
	isConductor(2*L/3+1,i)=.TRUE.
enddo
end subroutine initialize_lattice

subroutine laplace(V,isConductor,L,epsilon)
implicit none
integer::L
logical,dimension(L,L)::isConductor
real(8),dimension(L,L)::V
real(8)::epsilon
integer::i,j,icount
real(8)::Vav,error,dV
icount=0
do while(.TRUE.)
	error=0.0D0
	do j=2,L-1
	do i=2,L-1
	if(.NOT.isConductor(i,j))then
		Vav=(V(i-1,j)+V(i+1,j)+V(i,j+1)+V(i,j-1))*0.25D0
		dV=DABS(V(i,j)-Vav)
			if(error.LT.dV)error=dV!maximumerror
			V(i,j)=Vav
	endif
	enddo
	enddo
icount=icount+1
print*,icount,'err=',error
if(error.LT.epsilon)return!returntomainprogram
enddo
end subroutine laplace

subroutine print_results(V,L)
implicit none
integer::L
real(8),dimension(L,L)::V
integer::i,j
open(unit=11,file='data')
do i=1,L
do j=1,L
write(11,*)i,j,V(i,j)
enddo
write(11,*)''!emptylineforgnuplot,separateisolines
enddo
end subroutine print_results
