!> Main entry point for MESC optimization.
!>
!> Reads optimization parameters from params_val.txt, calls the appropriate
!> model function via the functor interface, and returns the cost function value.
!
program testvmic
    use mesc_precision_module, only: dp
    use mesc_function_module, only: functn

    implicit none

    real(dp) :: xparam(16),xcost,fa  !! optimization params, target cost, computed cost
    integer :: nx                    !! number of optimization parameters

    nx=6
    open(20,file="params_val.txt")
    read(20,*) xcost,xparam(1:nx)
    close(20)

    fa = functn(nx,xparam)
    print *, "cost12", xcost,xparam(1:nx),fa

end program testvmic
