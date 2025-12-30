program testvmic
      implicit real*8 (a-h,o-z)
!  ARRAYS FROM THE INPUT DATA
      real*8 xparam(16),xcost
      integer nx

      nx=6  
      open(20,file='params_val.txt')
      read(20,*) xcost,xparam(1:nx)
      close(20)

!      xparam(:) = 1.0
      fa = functn(nx,xparam)
!  END OF THE MAIN PROGRAM
      print *, 'cost12', xcost,xparam(1:nx),fa
      stop
end program testvmic
