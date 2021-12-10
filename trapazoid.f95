!trapazoid
      program trapazoid
              implicit none
              integer i,n
              real S1,a,b,f,h,summ,x,ana,area
              print*,"GIve the Value of a and b and interval"
              read*,a,b,n
              S1=(f(a)+f(b))/2
              h=(b-a)/n
              ana=(b**4-a**4)/4
              summ=0
              do i=1,n-1
                x=a+i*h
                summ=summ+f(x)
              enddo
              area=(summ+S1)*h
              print*,"Integration of given function x**3 by Trapazoid method",(summ+S1)*h
              write(*,5) abs((ana-area)/ana)*100
              5 format (1X,f9.7)
              end

        real function f(x)
                implicit none
                real x
                f=x**3
                end function
