!This is the fortran program for the simpson1/3
      program simpson
              implicit none
              real A1,A2,A3,a,b,f,x1,x2,Area,ana,h
              integer i,j,n
              A1=f(a)+f(b)
              A2=0
              A3=0
              write(*,*)"Give the initial value, final and total number of interval"
              read*,a,b,n
              ana=abs(b**4-a**4)/4
              h=abs(b-a)/n
              i=2
              do while(i.lt.n)
              !print*,i
                x1=a+i*h
                A2=A2+2*f(x1)
                !print*,A2
              	i=i+2
              enddo
              j=3
              do while(j.lt.n)
              !print*,j
                x2=a+j*h
                A3=A3+4*f(x2)
               	j=j+2
              enddo
              Area=(h/3)*(A1+A2+A3)
              print*,"Area under curve or integration by simpson method=",Area
              print*,"Error=",abs((Area-ana)/ana)*100
              end
                
              real function f(x)
                      real x
                      f=x**3
                      end function



