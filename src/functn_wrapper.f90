real*8 function functn(nx,xparam16)
  use function_module, only: functn_mod => functn
  implicit none
  integer, intent(in) :: nx
  real*8, intent(in)  :: xparam16(16)

  write(*,*) 'Entered wrapper functn, nx=', nx
  write(*,*) 'xparam16(1:min(nx,5))=', xparam16(1:min(nx,5))

  functn = functn_mod(nx,xparam16)

  write(*,*) 'Leaving wrapper functn, value=', functn
end function functn