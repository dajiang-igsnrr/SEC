!> The core routines for the mesc_model
!! calculate model parameters as function of enrvironmental variablles that vary with time
!! model integration using rk4
!!
! ##############mesc_model.f90###########################   
    subroutine rk4modelx(timex,delty,ny,isoc14,np,ns,kinetics,micpdef,micparam,micinput,xpool0,xpool1)
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_param_default), INTENT(IN)  :: micpdef
    TYPE(mic_parameter),     INTENT(IN)  :: micparam
    TYPE(mic_input),         INTENT(IN)  :: micinput
    integer      np,ns, kinetics,ny,isoc14
    real(r_2)    timex,delty,h
    real(r_2),   dimension(mcpool),intent(inout)     :: xpool0,xpool1
    real(r_2),   dimension(mcpool)                   :: y1,y2,y3,y4,dy1dt,dy2dt,dy3dt,dy4dt

     h=delty
     y1(:) = xpool0(:)
    
     call vmic_c(ny,isoc14,np,ns,kinetics,micpdef,micparam,micinput,y1,dy1dt)
     y2(:) = y1(:) + 0.5 * h * dy1dt(:)
     call vmic_c(ny,isoc14,np,ns,kinetics,micpdef,micparam,micinput,y2,dy2dt)
     y3(:) = y1(:) + 0.5 * h * dy2dt(:)
     call vmic_c(ny,isoc14,np,ns,kinetics,micpdef,micparam,micinput,y3,dy3dt)
     y4(:) = y1(:) +       h * dy3dt(:)
     call vmic_c(ny,isoc14,np,ns,kinetics,micpdef,micparam,micinput,y4,dy4dt)
    ! RK4
     xpool1(:) = xpool0(:) + (dy1dt(:)/6.0 + dy2dt(:)/3.0 + dy3dt(:)/3.0 + dy4dt(:)/6.0) * h
     
    ! Euler 
    ! xpool1(:) = xpool0(:) + dy1dt(:) * h
     
     
!    write(*,101) np,ns,delty,micinput%cinputm(np,ns)+micinput%cinputs(np,ns),sum(dy1dt(1:7)), &
!                 micinput%cinputm(np,ns)+micinput%cinputs(np,ns)-sum(dy1dt(1:7))
101 format('rk4: input, sumdelc rsoil',2(i3,1x),f6.2,1x,3(f10.6,1x)) 

    end subroutine rk4modelx

    subroutine Kmt(micpxdef,micpdef,micparam,micinput)
      ! unit: mg Mic C/cm3
      use mic_constant
      use mic_variable
      implicit none
      TYPE(mic_param_xscale), INTENT(IN)       :: micpxdef
      TYPE(mic_param_default), INTENT(IN)      :: micpdef
      TYPE(mic_parameter),     INTENT(INOUT)   :: micparam
      TYPE(mic_input),         INTENT(IN)      :: micinput

  
      ! local variable
      real(r_2), dimension(:,:), allocatable   :: xkclay,km,kmx
      integer nopt,np,ns

     allocate(xkclay(mp,ms),km(mp,ms),kmx(mp,ms))
     do np=1,mp
      do ns=1,ms
         nopt=micparam%bgctype(np)
         xkclay(np,ns) = 1.0/(2.0*exp(-2.0*sqrt(micinput%clay(np,ns))))
         km(np,ns) =  micpxdef%xak(nopt) * micpdef%ak * exp(micpdef%sk * micinput%tavg(np,ns) + micpdef%bk)
         micparam%K1(np,ns) =  km(np,ns)/micpdef%xk1
         micparam%K3(np,ns) =  km(np,ns) * xkclay(np,ns)/micpdef%xk3
         micparam%J1(np,ns) =  km(np,ns)/micpdef%xj1
         micparam%J3(np,ns) =  km(np,ns) * xkclay(np,ns)/micpdef%xj3

         kmx(np,ns) =  micpxdef%xak(nopt) * micpdef%ak * exp(micpdef%skx * micinput%tavg(np,ns) + micpdef%bk)       
         micparam%K2(np,ns) =  kmx(np,ns)/micpdef%xk2
         micparam%J2(np,ns) =  kmx(np,ns)/micpdef%xj2
       enddo
      enddo

       if(diag==1.and.np==outp) then   
          print *, 'Kmt',micinput%clay(outp,1),micinput%tavg(outp,1),km(outp,1),kmx(outp,1)
          print *, 'K1=',micparam%K1(outp,1)
          print *, 'K2=',micparam%K2(outp,1)
          print *, 'K3=',micparam%K3(outp,1)
          print *, 'J1=',micparam%J1(outp,1)
          print *, 'J2=',micparam%J2(outp,1)
          print *, 'J3=',micparam%J3(outp,1)   
       endif
      deallocate(xkclay,km,kmx)  
      
    end subroutine Kmt


    subroutine Kmt_single(micpxdef,micpdef,micparam,micinput,np)
      ! unit: mg Mic C/cm3
      use mic_constant
      use mic_variable
      implicit none
      TYPE(mic_param_xscale),  INTENT(IN)      :: micpxdef
      TYPE(mic_param_default), INTENT(IN)      :: micpdef
      TYPE(mic_parameter),     INTENT(INOUT)   :: micparam
      TYPE(mic_input),         INTENT(IN)      :: micinput
      integer,                 INTENT(IN)      :: np
  
      ! local variable
      real(r_2), dimension(:,:), allocatable   :: xkclay,km,kmx
      integer nopt,ns

      allocate(xkclay(mp,ms),km(mp,ms),kmx(mp,ms))
      do ns=1,ms
         nopt=micparam%bgctype(np)
         xkclay(np,ns) = 1.0/(2.0*exp(-2.0*sqrt(micinput%clay(np,ns))))
         km(np,ns) =  micpxdef%xak(nopt) * micpdef%ak * exp(micpdef%sk * micinput%tavg(np,ns) + micpdef%bk)
         micparam%K1(np,ns) =  km(np,ns)/micpdef%xk1
         micparam%K3(np,ns) =  km(np,ns) * xkclay(np,ns)/micpdef%xk3
         micparam%J1(np,ns) =  km(np,ns)/micpdef%xj1
         micparam%J3(np,ns) =  km(np,ns) * xkclay(np,ns)/micpdef%xj3

         kmx(np,ns) =  micpxdef%xak(nopt) * micpdef%ak * exp(micpdef%skx * micinput%tavg(np,ns) + micpdef%bk)       
         micparam%K2(np,ns) =  kmx(np,ns)/micpdef%xk2
         micparam%J2(np,ns) =  kmx(np,ns)/micpdef%xj2
      enddo

      if(diag==1.and.np==outp) then   
         print *, 'Kmt',micinput%clay(outp,1),micinput%tavg(outp,1),km(outp,1),kmx(outp,1)
         print *, 'K1=',micparam%K1(outp,1)
         print *, 'K2=',micparam%K2(outp,1)
         print *, 'K3=',micparam%K3(outp,1)
         print *, 'J1=',micparam%J1(outp,1)
         print *, 'J2=',micparam%J2(outp,1)
         print *, 'J3=',micparam%J3(outp,1)   
      endif
      deallocate(xkclay,km,kmx)  
      
    end subroutine Kmt_single


    subroutine Vmaxt(micpxdef,micpdef,micparam,micinput)
      ! mg Cs per mg mic C per hour
      use mic_constant
      use mic_variable
      implicit none
      TYPE(mic_param_xscale),  INTENT(IN)     :: micpxdef
      TYPE(mic_param_default), INTENT(IN)     :: micpdef
      TYPE(mic_parameter),     INTENT(INOUT)  :: micparam
      TYPE(mic_input),         INTENT(IN)     :: micinput
  

      ! local variables
      real(r_2),dimension(:,:), allocatable :: vmax
      integer nopt,np,ns
      real(r_2), dimension(:), allocatable   :: sdepthz

      allocate(vmax(mp,ms))
      allocate(sdepthz(ms))
      
      sdepthz=0.0
  
      do np=1,mp
           sdepthz=0.0
           nopt=micparam%bgctype(np)
          do ns=1,ms 
            if(ns==1) then
               sdepthz(ns) = 0.5 * micparam%sdepth(np,ns)
            else
                sdepthz(ns) = sdepthz(ns-1) + micparam%sdepth(np,ns)
            endif  
        !   vmax(np,ns) =  micpxdef%xav * micpdef%av * exp(micpdef%sv*micinput%tavg(np,ns) + micpdef%bv) * delt
        !   vmax(np,ns) =  exp(-2.0* sdepthz(ns)) * micpxdef%xav(npft) * micpdef%av * exp(micpdef%sv*micinput%tavg(np,ns) + micpdef%bv) * delt
           vmax(np,ns) =  exp(-micpdef%vmaxbeta * micpxdef%xvmaxbeta(nopt) * sdepthz(ns))     &
                                 * micpxdef%xav(nopt) * micpdef%av * exp(micpdef%sv*micinput%tavg(np,ns) + micpdef%bv)  * delt

           micparam%V1(np,ns)   =  micpdef%xv1 * vmax(np,ns) 
           micparam%V2(np,ns)   =  micpdef%xv2 * vmax(np,ns) 
           micparam%V3(np,ns)   =  micpdef%xv3 * vmax(np,ns) 
      
           micparam%W1(np,ns)   =  micpdef%xw1 * vmax(np,ns) 
           micparam%W2(np,ns)   =  micpdef%xw2 * vmax(np,ns)  
           micparam%W3(np,ns)   =  micpdef%xw3 * vmax(np,ns) 
          enddo
       enddo
         
        if(diag==1.and.np==outp) then 
           print *, 'Vmaxt',micinput%tavg(outp,1),vmax(outp,1)
           print *, 'V1=',micparam%V1(outp,1)
           print *, 'V2=',micparam%V2(outp,1)
           print *, 'V3=',micparam%V3(outp,1)
           print *, 'W1=',micparam%W1(outp,1)
           print *, 'W2=',micparam%W2(outp,1)
           print *, 'W3=',micparam%W3(outp,1)
        endif

      deallocate(vmax)
      deallocate(sdepthz)
      
    end subroutine Vmaxt


    subroutine Vmaxt_single(micpxdef,micpdef,micparam,micinput,np)
      ! mg Cs per mg mic C per hour
      use mic_constant
      use mic_variable
      implicit none
      TYPE(mic_param_xscale),  INTENT(IN)     :: micpxdef
      TYPE(mic_param_default), INTENT(IN)     :: micpdef
      TYPE(mic_parameter),     INTENT(INOUT)  :: micparam
      TYPE(mic_input),         INTENT(IN)     :: micinput
      integer,                 INTENT(IN)     :: np

      ! local variables
      real(r_2),dimension(:,:), allocatable :: vmax
      integer nopt,ns
      real(r_2), dimension(:), allocatable   :: sdepthz

      allocate(vmax(mp,ms))
      allocate(sdepthz(ms))
      
      sdepthz=0.0
  
 
      sdepthz=0.0
      nopt=micparam%bgctype(np)
      do ns=1,ms 
         if(ns==1) then
            sdepthz(ns) = 0.5 * micparam%sdepth(np,ns)
         else
            sdepthz(ns) = sdepthz(ns-1) + micparam%sdepth(np,ns)
         endif  
      !   vmax(np,ns) =  micpxdef%xav * micpdef%av * exp(micpdef%sv*micinput%tavg(np,ns) + micpdef%bv) * delt
      !   vmax(np,ns) =  exp(-2.0* sdepthz(ns)) * micpxdef%xav(npft) * micpdef%av * exp(micpdef%sv*micinput%tavg(np,ns) + micpdef%bv) * delt
         vmax(np,ns) =  exp(-micpdef%vmaxbeta * micpxdef%xvmaxbeta(nopt) * sdepthz(ns))     &
                              * micpxdef%xav(nopt) * micpdef%av * exp(micpdef%sv*micinput%tavg(np,ns) + micpdef%bv)  * delt

         micparam%V1(np,ns)   =  micpdef%xv1 * vmax(np,ns) 
         micparam%V2(np,ns)   =  micpdef%xv2 * vmax(np,ns) 
         micparam%V3(np,ns)   =  micpdef%xv3 * vmax(np,ns) 

         micparam%W1(np,ns)   =  micpdef%xw1 * vmax(np,ns) 
         micparam%W2(np,ns)   =  micpdef%xw2 * vmax(np,ns)  
         micparam%W3(np,ns)   =  micpdef%xw3 * vmax(np,ns) 
      enddo

         
      if(diag==1.and.np==outp) then 
         print *, 'Vmaxt',micinput%tavg(outp,1),vmax(outp,1)
         print *, 'V1=',micparam%V1(outp,1)
         print *, 'V2=',micparam%V2(outp,1)
         print *, 'V3=',micparam%V3(outp,1)
         print *, 'W1=',micparam%W1(outp,1)
         print *, 'W2=',micparam%W2(outp,1)
         print *, 'W3=',micparam%W3(outp,1)
      endif

      deallocate(vmax)
      deallocate(sdepthz)
      
    end subroutine Vmaxt_single


    subroutine Desorpt(micpxdef,micpdef,micparam,micinput)
      use mic_constant
      use mic_variable
      implicit none
!      real(r_2)              xdesorp
      TYPE(mic_param_xscale),  INTENT(IN)     :: micpxdef
      TYPE(mic_param_default), INTENT(IN)     :: micpdef
      TYPE(mic_parameter), INTENT(INOUT)      :: micparam 
      TYPE(mic_input), INTENT(IN)             :: micinput
      integer nopt,np,ns 

     do np=1,mp
      do ns=1,ms 
         nopt=micparam%bgctype(np)
         micparam%desorp(np,ns) = micpxdef%xdesorp(nopt) * (1.5e-5) * exp(-1.5*micinput%clay(np,ns)) 
      enddo
     enddo

      if(diag==1.and. np==outp) then
         print *, 'Desorpt'
         print *, 'desorpt=',micparam%desorp(outp,:)
      endif
  
    end subroutine Desorpt


    subroutine Desorpt_single(micpxdef,micparam,micinput,np)
      use mic_constant
      use mic_variable
      implicit none
!      real(r_2)              xdesorp
!,micpdef
      TYPE(mic_param_xscale),  INTENT(IN)     :: micpxdef
!      TYPE(mic_param_default), INTENT(IN)     :: micpdef
      TYPE(mic_parameter), INTENT(INOUT)      :: micparam 
      TYPE(mic_input), INTENT(IN)             :: micinput
      integer,                 INTENT(IN)    :: np
      integer nopt,ns 

  
      do ns=1,ms 
         nopt=micparam%bgctype(np)
         micparam%desorp(np,ns) = micpxdef%xdesorp(nopt) * (1.5e-5) * exp(-1.5*micinput%clay(np,ns)) 
      enddo


      if(diag==1.and. np==outp) then
         print *, 'Desorpt'
         print *, 'desorpt=',micparam%desorp(outp,:)
      endif
  
    end subroutine Desorpt_single


  subroutine mget(micpdef,micparam,micinput,micnpool)
     use mic_constant
     use mic_variable
     implicit none
     TYPE(mic_param_default), INTENT(IN)     :: micpdef
     TYPE(mic_parameter),     INTENT(INOUT)  :: micparam 
     TYPE(mic_input),         INTENT(IN)     :: micinput
     TYPE(mic_npool),         INTENT(IN)     :: micnpool 

     ! local variables
     integer np,ns

      do np=1,mp
       do ns=1,ms 
          ! variable mge 

         !  micparam%mgeR1(np,ns) = micpdef%cuemax*min(1.0,(micparam%cn_r(np,ns,1)/micparam%cn_r(np,ns,3)) &
         !                          **(micpdef%cue_coef1*(micnpool%mineralN(np,ns)-micpdef%cue_coef2)))

         !  micparam%mgeR2(np,ns) = micpdef%cuemax*min(1.0,(micparam%cn_r(np,ns,2)/micparam%cn_r(np,ns,3)) &
         !                          **(micpdef%cue_coef1*(micnpool%mineralN(np,ns)-micpdef%cue_coef2)))

         !  micparam%mgeR3(np,ns) = micpdef%cuemax*min(1.0,(micparam%cn_r(np,ns,7)/micparam%cn_r(np,ns,3)) &
         !                          **(micpdef%cue_coef1*(micnpool%mineralN(np,ns)-micpdef%cue_coef2)))

         !  micparam%mgeK1(np,ns) = micpdef%cuemax*min(1.0,(micparam%cn_r(np,ns,1)/micparam%cn_r(np,ns,4)) &
         !                          **(micpdef%cue_coef1*(micnpool%mineralN(np,ns)-micpdef%cue_coef2)))

         !  micparam%mgeK2(np,ns) = micpdef%cuemax*min(1.0,(micparam%cn_r(np,ns,2)/micparam%cn_r(np,ns,4)) &
         !                          **(micpdef%cue_coef1*(micnpool%mineralN(np,ns)-micpdef%cue_coef2)))

         !  micparam%mgeK3(np,ns) = micpdef%cuemax*min(1.0,(micparam%cn_r(np,ns,7)/micparam%cn_r(np,ns,4)) &
         !                          **(micpdef%cue_coef1*(micnpool%mineralN(np,ns)-micpdef%cue_coef2)))
         ! fixed mge
          micparam%mgeR1(np,ns) = micpdef%epislon1 * exp(-0.015 *micinput%tavg(np,ns))
          micparam%mgeR2(np,ns) = micpdef%epislon2 * exp(-0.015 *micinput%tavg(np,ns))
          micparam%mgeR3(np,ns) = micpdef%epislon1 * exp(-0.015 *micinput%tavg(np,ns))
          micparam%mgeK1(np,ns) = micpdef%epislon3 * exp(-0.015 *micinput%tavg(np,ns))
          micparam%mgeK2(np,ns) = micpdef%epislon4 * exp(-0.015 *micinput%tavg(np,ns))
          micparam%mgeK3(np,ns) = micpdef%epislon3 * exp(-0.015 *micinput%tavg(np,ns))
       enddo
      enddo
       
       if(diag==1.and.np==outp) then
          print *, 'mget'
          print *, 'epislon1-4=',micpdef%epislon1,micpdef%epislon2,micpdef%epislon3,micpdef%epislon4
       endif
        
  end subroutine mget


  subroutine mget_single(micpdef,micparam,micinput,micnpool,np)
     use mic_constant
     use mic_variable
     implicit none
     TYPE(mic_param_default), INTENT(IN)     :: micpdef
     TYPE(mic_parameter),     INTENT(INOUT)  :: micparam 
     TYPE(mic_input),         INTENT(IN)     :: micinput
     TYPE(mic_npool),         INTENT(IN)     :: micnpool 
     integer,                 INTENT(IN)     :: np

     ! local variables
     integer ns

      
      do ns=1,ms 
         ! variable mge 

      !  micparam%mgeR1(np,ns) = micpdef%cuemax*min(1.0,(micparam%cn_r(np,ns,1)/micparam%cn_r(np,ns,3)) &
      !                          **(micpdef%cue_coef1*(micnpool%mineralN(np,ns)-micpdef%cue_coef2)))

      !  micparam%mgeR2(np,ns) = micpdef%cuemax*min(1.0,(micparam%cn_r(np,ns,2)/micparam%cn_r(np,ns,3)) &
      !                          **(micpdef%cue_coef1*(micnpool%mineralN(np,ns)-micpdef%cue_coef2)))

      !  micparam%mgeR3(np,ns) = micpdef%cuemax*min(1.0,(micparam%cn_r(np,ns,7)/micparam%cn_r(np,ns,3)) &
      !                          **(micpdef%cue_coef1*(micnpool%mineralN(np,ns)-micpdef%cue_coef2)))

      !  micparam%mgeK1(np,ns) = micpdef%cuemax*min(1.0,(micparam%cn_r(np,ns,1)/micparam%cn_r(np,ns,4)) &
      !                          **(micpdef%cue_coef1*(micnpool%mineralN(np,ns)-micpdef%cue_coef2)))

      !  micparam%mgeK2(np,ns) = micpdef%cuemax*min(1.0,(micparam%cn_r(np,ns,2)/micparam%cn_r(np,ns,4)) &
      !                          **(micpdef%cue_coef1*(micnpool%mineralN(np,ns)-micpdef%cue_coef2)))

      !  micparam%mgeK3(np,ns) = micpdef%cuemax*min(1.0,(micparam%cn_r(np,ns,7)/micparam%cn_r(np,ns,4)) &
      !                          **(micpdef%cue_coef1*(micnpool%mineralN(np,ns)-micpdef%cue_coef2)))
      ! fixed mge
         micparam%mgeR1(np,ns) = micpdef%epislon1 * exp(-0.015 *micinput%tavg(np,ns))
         micparam%mgeR2(np,ns) = micpdef%epislon2 * exp(-0.015 *micinput%tavg(np,ns))
         micparam%mgeR3(np,ns) = micpdef%epislon1 * exp(-0.015 *micinput%tavg(np,ns))
         micparam%mgeK1(np,ns) = micpdef%epislon3 * exp(-0.015 *micinput%tavg(np,ns))
         micparam%mgeK2(np,ns) = micpdef%epislon4 * exp(-0.015 *micinput%tavg(np,ns))
         micparam%mgeK3(np,ns) = micpdef%epislon3 * exp(-0.015 *micinput%tavg(np,ns))
      enddo
      
      if(diag==1.and.np==outp) then
         print *, 'mget'
         print *, 'epislon1-4=',micpdef%epislon1,micpdef%epislon2,micpdef%epislon3,micpdef%epislon4
      endif
        
  end subroutine mget_single


  subroutine turnovert(kinetics,micpxdef,micpdef,micparam,micinput)
      use mic_constant
      use mic_variable
      implicit none
      TYPE(mic_param_xscale),  INTENT(IN)      :: micpxdef
      TYPE(mic_param_default), INTENT(IN)      :: micpdef
      TYPE(mic_parameter),     INTENT(INOUT)   :: micparam
      TYPE(mic_input),         INTENT(IN)      :: micinput  

      integer nx,kinetics
      real(r_2)  xbeta
 
      ! local variable
      integer nopt,np,ns
      real(r_2), dimension(:), allocatable    :: tvref

      allocate(tvref(mp)) 
       do np=1,mp
           nopt=micparam%bgctype(np)
           tvref(np) = sqrt(micinput%fcnpp(np)/micpdef%xtv)
           tvref(np) = max(0.6,min(1.3,tvref(np)))          ! 0.8-1.2 based on Wieder et al., 2015

  !         if(kinetics==3) then
  !            tvref(np) = 1.0
  !            tvref(np) = 1.0           
  !         endif

           do ns=1,ms
              micparam%tvmicR(np,ns)   = micpxdef%xtvmic(nopt) * micpdef%tvmicR * tvref(np) * exp(0.3 * micparam%fmetave(np,ns)) * delt
              micparam%tvmicK(np,ns)   = micpxdef%xtvmic(nopt) * micpdef%tvmicK * tvref(np) * exp(0.1 * micparam%fmetave(np,ns)) * delt
              micparam%betamicR(np,ns) = micpdef%betamic * micpxdef%xbeta(nopt)
              micparam%betamicK(np,ns) = micpdef%betamic * micpxdef%xbeta(nopt)
           enddo
       enddo

        if(diag==1.and.np==outp) then
          print *, 'turnovert'
          print *, 'tvmicR=',micparam%tvmicR(outp,:) 
          print *, 'tvmicR=',micparam%tvmicR(outp,:) 
        endif
      deallocate(tvref) 
  end subroutine turnovert


  subroutine turnovert_single(kinetics,micpxdef,micpdef,micparam,micinput,np)
      use mic_constant
      use mic_variable
      implicit none
      TYPE(mic_param_xscale),  INTENT(IN)      :: micpxdef
      TYPE(mic_param_default), INTENT(IN)      :: micpdef
      TYPE(mic_parameter),     INTENT(INOUT)   :: micparam
      TYPE(mic_input),         INTENT(IN)      :: micinput  
      integer,                 INTENT(IN)      :: np

      integer nx,kinetics
      real(r_2)  xbeta
 
      ! local variable
      integer nopt,ns
      real(r_2), dimension(:), allocatable    :: tvref

      allocate(tvref(mp)) 
      nopt=micparam%bgctype(np)
      tvref(np) = sqrt(micinput%fcnpp(np)/micpdef%xtv)
      tvref(np) = max(0.6,min(1.3,tvref(np)))          ! 0.8-1.2 based on Wieder et al., 2015

!         if(kinetics==3) then
!            tvref(np) = 1.0
!            tvref(np) = 1.0           
!         endif

      do ns=1,ms
         micparam%tvmicR(np,ns)   = micpxdef%xtvmic(nopt) * micpdef%tvmicR * tvref(np) * exp(0.3 * micparam%fmetave(np,ns)) * delt
         micparam%tvmicK(np,ns)   = micpxdef%xtvmic(nopt) * micpdef%tvmicK * tvref(np) * exp(0.1 * micparam%fmetave(np,ns)) * delt
         micparam%betamicR(np,ns) = micpdef%betamic * micpxdef%xbeta(nopt)
         micparam%betamicK(np,ns) = micpdef%betamic * micpxdef%xbeta(nopt)
      enddo


      if(diag==1.and.np==outp) then
         print *, 'turnovert'
         print *, 'tvref fmetave =', tvref(np),micparam%fmetave(np,:)
         print *, 'xtvmic xbeta = ', micpxdef%xtvmic(micparam%bgctype(np)),micpxdef%xbeta(micparam%bgctype(np))
         print *, 'tvmicR=',micparam%tvmicR(outp,:) 
         print *, 'tvmicR=',micparam%tvmicR(outp,:) 
      endif
      deallocate(tvref) 
  end subroutine turnovert_single



    subroutine bgc_fractions(micpxdef,micpdef,micparam,micinput)
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_param_xscale), INTENT(IN)      :: micpxdef
    TYPE(mic_param_default), INTENT(IN)     :: micpdef
    TYPE(mic_parameter),     INTENT(INOUT)  :: micparam  
    TYPE(mic_input),         INTENT(INOUT)  :: micinput
    !local variables
    integer npft,np,ns
    real(r_2), dimension(:),     allocatable :: fmetleaf,fmetroot,fmetwood
    real(r_2), dimension(:,:),   allocatable :: dleafx,drootx,dwoodx
    real(r_2), dimension(:,:),   allocatable :: cinputm,cinputs
    real(r_2), dimension(:,:,:), allocatable :: cninp
    
    
     allocate(fmetleaf(mp),fmetroot(mp),fmetwood(mp))
     allocate(dleafx(mp,ms),drootx(mp,ms),dwoodx(mp,ms))
     allocate(cinputm(mp,ms),cinputs(mp,ms))
     allocate(cninp(mp,ms,2))

     do np=1,mp
          npft=micparam%pft(np)
          
          fmetleaf(np) = max(0.0, 1.0 * (0.85 - 0.013 * micparam%fligleaf(np) * micparam%xcnleaf(np)))
          fmetroot(np) = max(0.0, 1.0 * (0.85 - 0.013 * micparam%fligroot(np) * micparam%xcnroot(np)))
          fmetwood(np) = max(0.0, 1.0 * (0.85 - 0.013 * micparam%fligwood(np) * micparam%xcnwood(np)))

          ! Initial C:N ratio of each C pool
          do ns=1,ms

             ! **this is a temporary solution, to be modified after N cycle is included
             micparam%cn_r(np,ns,1) = max( 5.0,0.5*(micparam%xcnleaf(np)+micparam%xcnroot(np)))
             micparam%cn_r(np,ns,2) = max(10.0,0.5*micparam%xcnleaf(np))
             micparam%cn_r(np,ns,3) =  7.4
             micparam%cn_r(np,ns,4) = 13.4
             micparam%cn_r(np,ns,5) = 12.0
             micparam%cn_r(np,ns,6) = 16.0
             micparam%cn_r(np,ns,7) = 10.0
 
 
       ! here zse in m, litter input in g/m2/delt, *0.001 to mgc/cm3/delt and "zse" in m.
             if(ns==1) then
                dleafx(np,ns) = micpxdef%xNPP(npft) * 0.001* micinput%dleaf(np)/micparam%sdepth(np,1)                               ! mgc/cm3/delt
                drootx(np,ns) = micpxdef%xNPP(npft) * 0.001* micparam%fracroot(np,1) * micinput%droot(np)/micparam%sdepth(np,1)     ! mgc/cm3/delt
                dwoodx(np,ns) = micpxdef%xNPP(npft) * 0.001* micinput%dwood(np)/micparam%sdepth(np,1)                               ! mgc/cm3/delt
             else
                dleafx(np,ns) = 0.0
                drootx(np,ns) = micpxdef%xNPP(npft) * 0.001 * micparam%fracroot(np,ns) * micinput%droot(np)/micparam%sdepth(np,ns)  ! mgc/cm3/delt
                dwoodx(np,ns) = 0.0
             endif

          !! calculate soil texture and litter quaility dependent parameter values
          ! C input to metabolic litter 
             micinput%cinputm(np,ns) = dleafx(np,ns)*fmetleaf(np)        &
                                     + drootx(np,ns)*fmetroot(np)        &       
                                     + dwoodx(np,ns)*fmetwood(np)         
          ! C input to structural litter
             micinput%cinputs(np,ns) = dleafx(np,ns)*(1.0-fmetleaf(np))  &
                                     + drootx(np,ns)*(1.0-fmetroot(np))  & 
                                     + dwoodx(np,ns)*(1.0-fmetwood(np)) 
 
          ! if((dleafx(np,ns)+drootx(np,ns))>0.0) then 
          ! C:N input of litter input to the metabolic pool 
                cninp(np,ns,1) = micinput%cinputm(np,ns)                          &
                               /(dleafx(np,ns)*fmetleaf(np)/micparam%xcnleaf(np)  &
                               +drootx(np,ns)*fmetroot(np)/micparam%xcnroot(np)   &
                               +dwoodx(np,ns)*fmetwood(np)/micparam%xcnwood(np))
          ! C:N input of litter input to the structural pool
                cninp(np,ns,2) = micinput%cinputs(np,ns)                               &
                               /(dleafx(np,ns)*(1.0-fmetleaf(np))/micparam%xcnleaf(np) &
                               +drootx(np,ns)*(1.0-fmetroot(np))/micparam%xcnroot(np)  &
                               +dwoodx(np,ns)*(1.0-fmetwood(np))/micparam%xcnwood(np))

                micparam%fmetave(np,ns) = (dleafx(np,ns)*fmetleaf(np) + drootx(np,ns)*fmetroot(np) + dwoodx(np,ns) * fmetwood(np))  &
                                        /(dleafx(np,ns) + drootx(np,ns) + dwoodx(np,ns) + 1.0e-10)

            !  else
            !    if(ns==1) then
            !       cninp(np,ns,1)          = micparam%xcnleaf(np)
            !       cninp(np,ns,2)          = micparam%xcnleaf(np)
            !       micparam%fmetave(np,ns) = fmetleaf(np)
            !    else
            !       cninp(np,ns,1)          = micparam%xcnroot(np)
            !       cninp(np,ns,2)          = micparam%xcnroot(np)
            !       micparam%fmetave(np,ns) = fmetroot(np)
            !    endif
            !  endif

             micparam%cn_r(np,ns,1) = cninp(np,ns,1); micparam%cn_r(np,ns,2)=cninp(np,ns,2)

            ! micparam%fr2p(np,ns) = micpdef%fmicsom1 * 0.30 * exp(1.3*micinput%clay(np,ns)) *1.0                   ! 3.0
            ! micparam%fk2p(np,ns) = micpdef%fmicsom2 * 0.20 * exp(0.8*micinput%clay(np,ns)) *1.0                   ! 3.0
            ! micparam%fr2c(np,ns) = min(1.0-micparam%fr2p(np,ns), micpdef%fmicsom3 * 0.10 * exp(-micpdef%fmicsom5 * micparam%fmetave(np,ns))*1.0 )    ! 9.0   to invoid a negative value of fr2a  ZHC
            ! micparam%fk2c(np,ns) = min(1.0-micparam%fk2p(np,ns), micpdef%fmicsom4 * 0.30 * exp(-micpdef%fmicsom5 * micparam%fmetave(np,ns))*1.0)     ! 9.0   to invoid a negative value of fk2a ZHC
            ! micparam%fr2a(np,ns) = 1.00 - micparam%fr2p(np,ns) - micparam%fr2c(np,ns)
            ! micparam%fk2a(np,ns) = 1.00 - micparam%fk2p(np,ns) - micparam%fk2c(np,ns)
            ! changes made to accommodate added aggregated pools

             micparam%fr2p(np,ns) =  0.30 * exp(1.3*micinput%clay(np,ns))                    ! 3.0
             micparam%fk2p(np,ns) =  0.20 * exp(0.8*micinput%clay(np,ns))                    ! 3.0
             micparam%fr2c(np,ns) = min(1.0, micparam%fr2p(np,ns) + 0.10 * exp(-3.0 * micparam%fmetave(np,ns)))     ! 9.0   to invoid a negative value of fr2a  ZHC
             micparam%fk2c(np,ns) = min(1.0, micparam%fk2p(np,ns) + 0.30 * exp(-3.0 * micparam%fmetave(np,ns)))     ! 9.0   to invoid a negative value of fk2a ZHC
             micparam%fr2p(np,ns) =  0.0
             micparam%fk2p(np,ns) =  0.0
             micparam%fr2a(np,ns) = max(0.0,1.00 - micparam%fr2c(np,ns))
             micparam%fk2a(np,ns) = max(0.0,1.00 - micparam%fk2c(np,ns))
          enddo   !"ns"
     enddo       !"np"

      if(diag==1.and.np ==outp) then
         print *,'bgc_fraction parameters'
         print *, 'empirical params1-4=', micpdef%fmicsom1,micpdef%fmicsom2,micpdef%fmicsom3,micpdef%fmicsom4
         print *, 'clay=', micinput%clay(np,:)
         print *, 'cinputm=', micinput%cinputm(outp,:)
         print *, 'cinputs=',micinput%cinputs(outp,:)
         print *, 'fmetave=',micparam%fmetave(outp,:)
         print *, 'cn_r1=',micparam%cn_r(outp,:,1) 
         print *, 'cn_r2=',micparam%cn_r(outp,:,2)
         print *, 'fr2p=',micparam%fr2p(outp,:) 
         print *, 'fk2p=',micparam%fk2p(outp,:) 
         print *, 'fr2c=',micparam%fr2c(outp,:)
         print *, 'fk2c=',micparam%fk2c(outp,:)
         print *, 'fr2a=',micparam%fr2a(outp,:) 
         print *, 'fk2a=',micparam%fk2a(outp,:)
      endif
     deallocate(fmetleaf,fmetroot,fmetwood)
     deallocate(dleafx,drootx,dwoodx)
     deallocate(cinputm,cinputs)
     deallocate(cninp)   
   
   end subroutine bgc_fractions


   subroutine bgc_fractions_single(micpxdef,micpdef,micparam,micinput,np)
      use mic_constant
      use mic_variable
      implicit none
      TYPE(mic_param_xscale),  INTENT(IN)     :: micpxdef
      TYPE(mic_param_default), INTENT(IN)     :: micpdef
      TYPE(mic_parameter),     INTENT(INOUT)  :: micparam  
      TYPE(mic_input),         INTENT(INOUT)  :: micinput
      integer,                 INTENT(IN)     :: np
      !local variables
      integer npft,ns
      real(r_2), dimension(:),     allocatable :: fmetleaf,fmetroot,fmetwood
      real(r_2), dimension(:,:),   allocatable :: dleafx,drootx,dwoodx
      real(r_2), dimension(:,:),   allocatable :: cinputm,cinputs
      real(r_2), dimension(:,:,:), allocatable :: cninp
      
      
      allocate(fmetleaf(mp),fmetroot(mp),fmetwood(mp))
      allocate(dleafx(mp,ms),drootx(mp,ms),dwoodx(mp,ms))
      allocate(cinputm(mp,ms),cinputs(mp,ms))
      allocate(cninp(mp,ms,2))


      npft=micparam%pft(np)
      fmetleaf(np) = max(0.0, 1.0 * (0.85 - 0.013 * micparam%fligleaf(np) * micparam%xcnleaf(np)))
      fmetroot(np) = max(0.0, 1.0 * (0.85 - 0.013 * micparam%fligroot(np) * micparam%xcnroot(np)))
      fmetwood(np) = max(0.0, 1.0 * (0.85 - 0.013 * micparam%fligwood(np) * micparam%xcnwood(np)))

      ! Initial C:N ratio of each C pool
      do ns=1,ms

         ! **this is a temporary solution, to be modified after N cycle is included
         micparam%cn_r(np,ns,1) = max( 5.0,0.5*(micparam%xcnleaf(np)+micparam%xcnroot(np)))
         micparam%cn_r(np,ns,2) = max(10.0,0.5*micparam%xcnleaf(np))
         micparam%cn_r(np,ns,3) =  7.4
         micparam%cn_r(np,ns,4) = 13.4
         micparam%cn_r(np,ns,5) = 12.0
         micparam%cn_r(np,ns,6) = 16.0
         micparam%cn_r(np,ns,7) = 10.0


         ! here zse in m, litter input in g/m2/delt, *0.001 to mgc/cm3/delt and "zse" in m.
         if(ns==1) then
            dleafx(np,ns) = micpxdef%xNPP(npft) * 0.001* micinput%dleaf(np)/micparam%sdepth(np,1)                               ! mgc/cm3/delt
            drootx(np,ns) = micpxdef%xNPP(npft) * 0.001* micparam%fracroot(np,1) * micinput%droot(np)/micparam%sdepth(np,1)     ! mgc/cm3/delt
            dwoodx(np,ns) = micpxdef%xNPP(npft) * 0.001* micinput%dwood(np)/micparam%sdepth(np,1)                               ! mgc/cm3/delt
         else
            dleafx(np,ns) = 0.0
            drootx(np,ns) = micpxdef%xNPP(npft) * 0.001 * micparam%fracroot(np,ns) * micinput%droot(np)/micparam%sdepth(np,ns)  ! mgc/cm3/delt
            dwoodx(np,ns) = 0.0
         endif

         !! calculate soil texture and litter quaility dependent parameter values
         ! C input to metabolic litter 
         micinput%cinputm(np,ns) = dleafx(np,ns)*fmetleaf(np)        &
                                 + drootx(np,ns)*fmetroot(np)        &       
                                 + dwoodx(np,ns)*fmetwood(np)         
         ! C input to structural litter
         micinput%cinputs(np,ns) = dleafx(np,ns)*(1.0-fmetleaf(np))  &
                                 + drootx(np,ns)*(1.0-fmetroot(np))  & 
                                 + dwoodx(np,ns)*(1.0-fmetwood(np)) 

         ! if((dleafx(np,ns)+drootx(np,ns))>0.0) then 
         ! C:N input of litter input to the metabolic pool 
         cninp(np,ns,1) = micinput%cinputm(np,ns)                          &
                        /(dleafx(np,ns)*fmetleaf(np)/micparam%xcnleaf(np)  &
                        +drootx(np,ns)*fmetroot(np)/micparam%xcnroot(np)   &
                        +dwoodx(np,ns)*fmetwood(np)/micparam%xcnwood(np))
         ! C:N input of litter input to the structural pool
         cninp(np,ns,2) = micinput%cinputs(np,ns)                               &
                        /(dleafx(np,ns)*(1.0-fmetleaf(np))/micparam%xcnleaf(np) &
                        +drootx(np,ns)*(1.0-fmetroot(np))/micparam%xcnroot(np)  &
                        +dwoodx(np,ns)*(1.0-fmetwood(np))/micparam%xcnwood(np))

         micparam%fmetave(np,ns) = (dleafx(np,ns)*fmetleaf(np) + drootx(np,ns)*fmetroot(np) + dwoodx(np,ns) * fmetwood(np))  &
                                 /(dleafx(np,ns) + drootx(np,ns) + dwoodx(np,ns) + 1.0e-10)

         !  else
         !    if(ns==1) then
         !       cninp(np,ns,1)          = micparam%xcnleaf(np)
         !       cninp(np,ns,2)          = micparam%xcnleaf(np)
         !       micparam%fmetave(np,ns) = fmetleaf(np)
         !    else
         !       cninp(np,ns,1)          = micparam%xcnroot(np)
         !       cninp(np,ns,2)          = micparam%xcnroot(np)
         !       micparam%fmetave(np,ns) = fmetroot(np)
         !    endif
         !  endif

         micparam%cn_r(np,ns,1) = cninp(np,ns,1); micparam%cn_r(np,ns,2)=cninp(np,ns,2)

         ! micparam%fr2p(np,ns) = micpdef%fmicsom1 * 0.30 * exp(1.3*micinput%clay(np,ns)) *1.0                   ! 3.0
         ! micparam%fk2p(np,ns) = micpdef%fmicsom2 * 0.20 * exp(0.8*micinput%clay(np,ns)) *1.0                   ! 3.0
         ! micparam%fr2c(np,ns) = min(1.0-micparam%fr2p(np,ns), micpdef%fmicsom3 * 0.10 * exp(-micpdef%fmicsom5 * micparam%fmetave(np,ns))*1.0 )    ! 9.0   to invoid a negative value of fr2a  ZHC
         ! micparam%fk2c(np,ns) = min(1.0-micparam%fk2p(np,ns), micpdef%fmicsom4 * 0.30 * exp(-micpdef%fmicsom5 * micparam%fmetave(np,ns))*1.0)     ! 9.0   to invoid a negative value of fk2a ZHC
         ! micparam%fr2a(np,ns) = 1.00 - micparam%fr2p(np,ns) - micparam%fr2c(np,ns)
         ! micparam%fk2a(np,ns) = 1.00 - micparam%fk2p(np,ns) - micparam%fk2c(np,ns)
         ! changes made to accommodate added aggregated pools

         micparam%fr2p(np,ns) =  0.30 * exp(1.3*micinput%clay(np,ns))                    ! 3.0
         micparam%fk2p(np,ns) =  0.20 * exp(0.8*micinput%clay(np,ns))                    ! 3.0
         micparam%fr2c(np,ns) = min(1.0, micparam%fr2p(np,ns) + 0.10 * exp(-3.0 * micparam%fmetave(np,ns)))     ! 9.0   to invoid a negative value of fr2a  ZHC
         micparam%fk2c(np,ns) = min(1.0, micparam%fk2p(np,ns) + 0.30 * exp(-3.0 * micparam%fmetave(np,ns)))     ! 9.0   to invoid a negative value of fk2a ZHC
         micparam%fr2p(np,ns) =  0.0
         micparam%fk2p(np,ns) =  0.0
         micparam%fr2a(np,ns) = max(0.0,1.00 - micparam%fr2c(np,ns))
         micparam%fk2a(np,ns) = max(0.0,1.00 - micparam%fk2c(np,ns))
      enddo   !"ns"


      if(diag==1.and.np ==outp) then
         print *,'bgc_fraction parameters and pft',micparam%pft(np)
         print *, 'empirical params1-4=', micpdef%fmicsom1,micpdef%fmicsom2,micpdef%fmicsom3,micpdef%fmicsom4
         print *, 'fligleaf,xcnleaf=', micparam%fligleaf(np),micparam%xcnleaf(np)
         print *, 'fracroot sdepth', micparam%fracroot(np,:),micparam%sdepth(np,:)
         print *, 'cinputm=', micinput%cinputm(outp,:)
         print *, 'cinputs=',micinput%cinputs(outp,:)
         print *, 'fmetave=',micparam%fmetave(outp,:)
         print *, 'cn_r1=',micparam%cn_r(outp,:,1) 
         print *, 'cn_r2=',micparam%cn_r(outp,:,2)
         print *, 'fr2p=',micparam%fr2p(outp,:) 
         print *, 'fk2p=',micparam%fk2p(outp,:) 
         print *, 'fr2c=',micparam%fr2c(outp,:)
         print *, 'fk2c=',micparam%fk2c(outp,:)
         print *, 'fr2a=',micparam%fr2a(outp,:) 
         print *, 'fk2a=',micparam%fk2a(outp,:)
      endif

      deallocate(fmetleaf,fmetroot,fmetwood)
      deallocate(dleafx,drootx,dwoodx)
      deallocate(cinputm,cinputs)
      deallocate(cninp)   
   
   end subroutine bgc_fractions_single


   subroutine bioturb(ndelt,ms,zse,delt,diffsocxx,fluxsoc,xpooli,xpoole)
   ! multi-layered soil BGC including DOC and bioturbation using microbially-based BGC modeling
   ! step 1: litter-C and SOC bioturbation treated as a diffusion process
   ! step 2: advection of DOC along with water flux
   ! solve dc/dt=Dd2c/dx +F(z) where c is total SOC concentration in each soil layer
   ! bioturbation diffusion rate 
   ! boundary conditions: at the top     -Ddc/dx = F0+F(1)  at x=0
   !                      at the bottom: dC/dx=0            at x=h
   ! using the fully implicit method together with Thomas method
   ! unit for pool:                 mgc/cm3      (=kg C/m3)
   !      for flux:                 mgc/cm3/delt (=kg c/m3/delt): g/m2/delt = 0.1 mg/cm2/delt
   !      for length:               cm
   !      for diffsion coefficient: cm2/delt
   use mic_constant,  ONLY : r_2
   implicit none
   integer                        ndelt,ms
   real(r_2), dimension(ms)    :: zse
   real(r_2)                      delt,diffsocxx
   real(r_2), dimension(ms)    :: xpooli,xpoole,xpool,fluxsoc
   ! local variables
   integer                        i,j
   real(r_2)                      deltD,tot0, tot1, totflux
   real(r_2), dimension(ms)    :: xzse
   real(r_2), dimension(ms+1)  :: sdepthx
   real(r_2)                      coeffA, coeffB
   real(r_2), dimension(ms)    :: at,bt,ct,rt

      ! calculate the mid-point of each layer
     sdepthx(1) = 0.0          ! depth of a layer from the top (x_0.5=0.0 eg soil surface)
     do j=2,ms+1
        sdepthx(j) = sdepthx(j-1) + zse(j-1)*100.0     ! depth of the bottom of each layer (eg x_j+0.5)
                                                       !*100 to convert from m to cm
     enddo

     do j=1,ms
        xzse(j) = 0.5 * (sdepthx(j) + sdepthx(j+1))    ! depth of midpoint of a layer j  (x_j)
     enddo

     deltD = diffsocxx * delt

      xpool = xpooli
      tot0 = 0.0
     do j=1,ms
        tot0 = tot0 + xpool(j) * zse(j)*100.0         !*100 convert m to cm
     enddo
  
     do i=1,ndelt
        do j=1,ms
           if(j==1) then
              coeffB = 1.0/(sdepthx(2)-sdepthx(1))
              coeffA = deltD*coeffB/(xzse(2)-xzse(1))
              ! Crank-Nicholson
              at(1) = 0.0
              bt(1) = 1.0 + 0.5 * coeffA
              ct(1) =     - 0.5 * coeffA
              rt(1) = (1.0-0.5*coeffA) * xpool(1) + 0.5 * coeffA * xpool(2) &
                    +  fluxsoc(1) * delt 
           endif
           if(j>1.and.j<ms) then
             coeffA = deltD/((xzse(j+1)-xzse(j))*(sdepthx(j+1)-sdepthx(j)))
             coeffB = (xzse(j+1)-xzse(j))/(xzse(j)-xzse(j-1))
             ! Crank-Nicholson
             at(j) =    -0.5 * coeffA * coeffB
             bt(j) = 1.0+0.5 * coeffA *(1.0+coeffB)
             ct(j) =    -0.5 * coeffA
             rt(j) = 0.5 * coeffA * coeffB * xpool(j-1)        &
                     +(1.0-0.5* coeffA*(1.0+coeffB))*xpool(j)  &
                     + 0.5* coeffA * xpool(j+1)                &
                     + fluxsoc(j) *delt
           endif
           if(j==ms) then
               coeffA = deltD/((xzse(ms)-xzse(ms-1))*(sdepthx(ms+1) - sdepthx(ms)))
             ! Crank-Nicholson
               at(ms) = -0.5 * coeffA
               bt(ms) = 1.0 + 0.5 * coeffA
               ct(ms) = 0.0
               rt(ms) = 0.5* coeffA  * xpool(ms-1) + (1.0-0.5 * coeffA) * xpool(ms) &
                    + fluxsoc(ms) * delt
            endif
        enddo
 
        call tridag(at,bt,ct,rt,xpool,ms)
     enddo
     xpoole = xpool
     
     tot1 = 0.0
     totflux=0.0
     do j=1,ms
        tot1 = tot1 + xpool(j) * zse(j) *100.0
        totflux = totflux + fluxsoc(j) * zse(j) *100.0
     enddo
  
end subroutine bioturb

   subroutine tridag(at,bt,ct,rt,u,ms)
   ! solving the triadigonal matrix (numerical recipes, p43)
   ! linear equation: A* u(i-1) + B *u(i) + C * u(i+1) = R, 
   ! where i is soil layer, u(i-1), u(i) and u(i+1) are at time step t
   ! NOTE: bt(1) should not be equal to 0.0, otherwise rewrite the equation
    use mic_constant,  ONLY : r_2
    implicit none
    integer, parameter    :: nmax=500
    integer ms
    real(r_2), dimension(ms)    :: at,bt,ct,rt,u
    integer j
    real(r_2) bet
    real(r_2), dimension(nmax) :: gam
     
      bet  = bt(1)
      u(1) = rt(1)/bet
      do j=2,ms
         gam(j) = ct(j-1)/bet
         bet = bt(j)-at(j)* gam(j)
         if(bet ==0) then
            print *, 'triag failed'
            stop
         endif
         u(j) = (rt(j) - at(j) * u(j-1))/bet
      enddo
      do j=ms-1,1,-1
         u(j) = u(j) -gam(j+1) * u(j+1)
      enddo
    end subroutine tridag


    subroutine advecdoc(deltx,zse,fluxsoilwx,fluxdocsx,vsoilwx,ypool)
    ! to be modified using an implicit solver to ensure mass conservation
    !
    use mic_constant
    implicit none
    real(r_2)                          deltx
    real(r_2), dimension(ms)        :: zse,fluxsoilwx,vsoilwx,ypool
    real(r_2), dimension(ms)        :: dypool,ypool1
    real(r_2)                          fluxdocsx,totdoc0,totdoc1,fluxdocbot 
    integer ns,iter
     
     ypool1= ypool
     fluxdocbot = 0.0
     do iter=1,100
      do ns=1,ms
        if(ns==1) then
           dypool(1)  = (fluxdocsx - fluxsoilwx(1)*ypool1(1)/vsoilwx(1))*deltx*0.01/zse(1)
        else
           dypool(ns) = (fluxsoilwx(ns-1)*ypool1(ns-1)/vsoilwx(ns-1) &
                        -fluxsoilwx(ns)  *ypool1(ns)/vsoilwx(ns))       *deltx*0.01/zse(ns)
        endif
        if(ns==ms) then
           fluxdocbot = fluxdocbot + fluxsoilwx(ns)  *ypool1(ns)/vsoilwx(ns) *deltx* 0.01
        endif
      enddo
      ypool1 = max(0.0,ypool1+dypool)
     enddo
     ! check mass conservation
     totdoc0=0.0; totdoc1=0.0
     do ns=1,ms
        totdoc0 = totdoc0 + ypool(ns)  *zse(ns)
        totdoc1 = totdoc1 + ypool1(ns) *zse(ns)
     enddo
    ! print *, 'mass cons DOC', totdoc0,totdoc1,(totdoc1-totdoc0)-(fluxdocsx - fluxdocbot)*deltx
    
     ypool = ypool1
     
    end subroutine advecdoc


    subroutine vmic_c(ny,isoc14,np,ns,kinetics,micpdef,micparam,micinput,xpool,y)
    ! MIMICS as modified by Zhang et al. (2019, GCB).
    ! Seven pools: metabolic litter (1), Structural litter, microbe-R (3), microbe-K(4),
    !              Physical protected (5), chemically-protected (6), active (7)
    ! for each layer
    ! input: aboveground litter and belowground in the surface layer
    !        belowground input in other layers
    ! kinetics: Michaelis-Mennten
    ! unit:
    ! all carbon pools : mg C/cm3
    ! time step :        one hour
    !
     use mic_constant
     use mic_variable
     implicit none
     
     TYPE(mic_param_default), INTENT(IN)     :: micpdef    
     TYPE(mic_parameter),     INTENT(IN)     :: micparam
     TYPE(mic_input),         INTENT(IN)     :: micinput

     real(r_2),  parameter                           ::  kamin = 0.2      !          Abramoff et al. (2022)
     real(r_2),  parameter                           ::  lamda = 2.01e-4  !1/kPa     Abramoof et al. (2022) 

     real(r_2),  dimension(mcpool),  INTENT(IN)        :: xpool 
     real(r_2),  dimension(mcpool),  INTENT(INOUT)     :: y   !=dxpool/dt     ! local variables
     ! local variables
     integer     np,ns,kinetics,ny,isoc14,ip  

     real(r_2)  betamicR,betamicK,                 &
                cinputmx,cinputsx,fmx,fsx,         &
                fr2px,fr2cx,fr2ax,                 &
                fk2px,fk2cx,fk2ax,                 &
                mgeRx1,mgeRx2,mgeRx3,              &
                mgeKx1,mgeKx2,mgeKx3,              &
                tvmicRx,tvmicKx,                   &
                tavgx,clayx,                       &
                desorpx,                           &
                V1x,V2x,V3x,W1x,W2x,W3x,           &
                J1x,J2x,J3x,K1x,K2x,K3x,           &
                Q1x,Q2x
                

     real(r_2) cfluxm2r, cfluxm2k, cfluxs2r, cfluxs2k, cfluxr,   cfluxk
     real(r_2) cfluxr2p, cfluxk2p, cfluxp2a, cfluxr2c, cfluxk2c
     real(r_2) cfluxc2a, cfluxr2a, cfluxk2a, cfluxa2r, cfluxa2k
	 
     ! additional variables for kinetics3
     real(r_2)  cfluxa,cfluxp,cfluxc2p,cfluxa2c,cfluxp2c
     real(r_2)  kadsorpx,kdesorpx,fp2ax,moistx,soilphx,porex,xwater,phx1,phx2,phx3,siltx,tvcpoolx,tvppoolx,tvacx         
     real(r_2)  smexpa,smopt,qmaxcoeffx,qmax
     real(r_2)  swbx,swdx,matpotx,xwater1,xwater2
     real(r_2)  rsoil
     real(r_2)  cfluxp2m,cfluxp2s 	 

      ! matpotx =-15.0 !dummy value for the time being

      if(isoc14==1) then

         if(ny<1) then
            cinputmx = micinput%cinputm(np,ns) *  micparam%c14atm(1,micparam%region(np),2)        ! using fraction modern before 1941
            cinputsx = micinput%cinputs(np,ns) *  micparam%c14atm(1,micparam%region(np),2)
         else
            cinputmx = micinput%cinputm(np,ns) *  micparam%c14atm(ny,micparam%region(np),2)       ! using fraction modern after 1941
            cinputsx = micinput%cinputs(np,ns) *  micparam%c14atm(ny,micparam%region(np),2)
         endif
      else
         cinputmx = micinput%cinputm(np,ns)
         cinputsx = micinput%cinputs(np,ns)
      endif
  
      tavgx    = micinput%tavg(np,ns);      clayx    = micinput%clay(np,ns);    siltx    = micinput%silt(np,ns)  

      fmx      = micparam%fm(np,ns);        fsx      = micparam%fs(np,ns)  
      fr2px    = micparam%fr2p(np,ns);      fr2cx    = micparam%fr2c(np,ns)
      fr2ax    = micparam%fr2a(np,ns);      fk2px    = micparam%fk2p(np,ns)
      fk2cx    = micparam%fk2c(np,ns);      fk2ax    = micparam%fk2a(np,ns)
      mgeRx1   = micparam%mgeR1(np,ns);     mgeRx2   = micparam%mgeR2(np,ns);   mgeRx3 = micparam%mgeR3(np,ns)
      mgeKx1   = micparam%mgeK1(np,ns);     mgeKx2   = micparam%mgeK2(np,ns);   mgeKx3 = micparam%mgeK3(np,ns)
      tvmicRx  = micparam%tvmicR(np,ns);    tvmicKx  = micparam%tvmicK(np,ns) 
      desorpx  = micparam%desorp(np,ns) 
      V1x      = micparam%V1(np,ns);        V2x      = micparam%V2(np,ns);      V3x    = micparam%V3(np,ns)
      W1x      = micparam%W1(np,ns);        W2x      = micparam%W2(np,ns);      W3x    = micparam%W3(np,ns)
      K1x      = micparam%K1(np,ns);        K2x      = micparam%K2(np,ns);      K3x    = micparam%K3(np,ns)
      J1x      = micparam%J1(np,ns);        J2x      = micparam%J2(np,ns);      J3x    = micparam%J3(np,ns)
      Q1x      = micparam%Q1(np,ns);        Q2x      = micparam%Q2(np,ns)            
      betamicR = micparam%betamicR(np,ns);  betamicK = micparam%betamicK(np,ns)


!      print *, 'p1=',  fmx, fsx 
!      print *, 'p2=',  fr2px,fr2cx    
!      print *, 'p3=',  fr2ax,fk2px 
!      print *, 'p4=',  fk2cx,fk2ax
!      print *, 'p5=',  mgeRx1,mgeRx2,mgeRx3 
!      print *, 'p6=',  mgeKx1,mgeKx2,mgeKx3 
!      print *, 'p7=',  tvmicRx,tvmicKx
!      print *, 'p8=',  desorpx
!      print *, 'p9=',  V1x,V2x,V3x
!      print *, 'p10=', W1x,W2x,W3x
!      print *, 'p11=', K1x,K2x,K3x
!      print *, 'p12=', J1x,J2x,J3x
!      print *, 'p13=', Q1x,Q2x       
!      print *, 'p14=', betamicR,betamicK    
  
      ! additional parameters and input for kinetics3
      if(kinetics==3) then
         moistx   = micinput%wavg(np,ns);          matpotx    = micinput%matpot(np,ns)
         soilphx  = micinput%ph(np,ns);            porex      = micinput%porosity(np,ns)
         kadsorpx = micparam%kadsorp(np,ns);       tvcpoolx   = micparam%tvcpool(np,ns);   tvppoolx =micparam%tvppool(np,ns) 
         tvacx    = micparam%tvac(np,ns);          fp2ax      = micparam%fp2a(np,ns)
         kdesorpx = micparam%kdesorp(np,ns);       qmaxcoeffx = micparam%qmaxcoeff(np,ns)
               
         ! we applied a single water-limiting function from Yan et al. 2018, Nature Coomunitation. eqn1)
         if(clayx <= 0.016) then
            smexpa=0.0
         else if(clayx >0.016 .and. clayx <=0.37) then
            smexpa=2.8* clayx-0.046
         else
            smexpa=1.0
         endif

         smopt     = 0.65 * porex

         if(moistx < smopt) then
            xwater = ((micpdef%smkdesorp+smopt)/(micpdef%smkdesorp + moistx))     &
                   * (moistx/smopt)**(1.0+smexpa *micpdef%smexpns)
     
         else
            xwater = ((porex - moistx)/(porex-smopt)) **micpdef%smexpb
         endif 
     

         ! soil water limitation from Abramoff et al. (2022) eqn (4) and eqn (15)
         xwater1 = sqrt(min(1.0,moistx/porex))                                                                ! eqn (4)
         xwater2 = exp(lamda * matpotx) *(kamin + (1.0- kamin) *sqrt(max(0.0,1.0-moistx/porex)))*xwater1      ! eqn (15)

         phx1      = exp(-micpdef%phcoeff1* soilphx - micpdef%phcoeff2)                          ! eqn(10) Abramoff2022
         phx2     = 1.0/(1.0+exp(-(soilphx-4.798)/0.4246))        !bacteria
         phx3     = 1.0/(1.0+exp(-(soilphx-3.022)/0.428))         !fungi
         qmax     = qmaxcoeffx * (clayx + siltx)*100.0  ! mg C/g soil, based on Georgiou et al. (2022), media value (their  Fig 1)
         ! unit conversion qmax to mg C/cm3
         qmax     = qmax * micinput%bulkd(np,ns) *0.001                        ! bulkd in kg/m3 multiply by 0.001 into g/cm3
     
         !    xwater = sqrt(micinput%moist(np,ns)/micinput%poros(np,ns))      ! eqn (4) Abramoff2022 
         !    xwmic    = xwdecomp * exp(lambda * (-micinput%matpot(np,ns)) * (swmin + (1.0-swmin(np,ns)) * &   
         !               ((micinput%poros(np) - micinput%moist(np,ns))/micinput%poros(np,ns)) **0.5)    !eqn(15) Abramoff2022
  
      endif
      ! carbon fluxes
      if(kinetics==1) then
        ! forward Michaelis-Menten
        cfluxm2r = xpool(3) * V1x * xpool(1)/(K1x + xpool(1))
        cfluxs2r = xpool(3) * V2x * xpool(2)/(K2x + xpool(2))
        cfluxa2r = xpool(3) * V3x * xpool(7)/(K3x + xpool(7))

        cfluxm2k = xpool(4) * W1x * xpool(1)/(J1x + xpool(1))
        cfluxs2k = xpool(4) * W2x * xpool(2)/(J2x + xpool(2))
        cfluxa2k = xpool(4) * W3x * xpool(7)/(J3x + xpool(7))

        cfluxr   = tvmicRx * xpool(3) ** betamicR
        cfluxk   = tvmicKx * xpool(4) ** betamicK

        cfluxr2p = fr2px * cfluxr
        cfluxk2p = fk2px * cfluxk

        cfluxr2c = fr2cx   * cfluxr 
        cfluxk2c = fk2cx   * cfluxk

        cfluxp2a = desorpx * xpool(5)
        cfluxr2a = fr2ax   * cfluxr
        cfluxk2a = fk2ax   * cfluxk
        cfluxc2a = xpool(3)* V2x * xpool(6)/(Q1x*K2x + xpool(6))   &
                 + xpool(4)* W2x * xpool(6)/(Q2x*J2x + xpool(6))
      endif
      if(kinetics ==2 )then 
        !=======================================================
        ! reverse Michaelis-Menten
        cfluxm2r = xpool(1) * V1x * xpool(3)/(K1x + xpool(3))
        cfluxs2r = xpool(2) * V2x * xpool(3)/(K2x + xpool(3))
        cfluxa2r = xpool(7) * V3x * xpool(3)/(K3x + xpool(3))

        cfluxm2k = xpool(1) * W1x * xpool(4)/(J1x + xpool(4))
        cfluxs2k = xpool(2) * W2x * xpool(4)/(J2x + xpool(4))
        cfluxa2k = xpool(7) * W3x * xpool(4)/(J3x + xpool(4))

        cfluxr   = tvmicRx * xpool(3) ** betamicR
        cfluxk   = tvmicKx * xpool(4) ** betamicK


        cfluxr2p = fr2px   * cfluxr
        cfluxk2p = fk2px   * cfluxk

        cfluxr2c = fr2cx * cfluxr 
        cfluxk2c = fk2cx * cfluxk

        cfluxp2a = desorpx * xpool(5)
        cfluxr2a = fr2ax * cfluxr
        cfluxk2a = fk2ax * cfluxk
        cfluxc2a = xpool(6) * V2x * xpool(3)/(Q1x*K2x + xpool(3))   &
                 + xpool(6) * W2x * xpool(4)/(Q2x*J2x + xpool(4))
      endif

      !===================================================
      !
      if(kinetics ==1 .or. kinetics==2) then      
         ! metabolic litter  [=Im*(1-fm)-A1-A5]
         y(1) = cinputmx * (1.0-fmx) - cfluxm2r - cfluxm2k

         ! structural litter [=Is*(1-fs)-A2-A6]
         y(2) = cinputsx * (1.0-fsx) - cfluxs2r - cfluxs2k
        
        ! these two are incorrect
        ! !microbe R          [mge1*A1+mge2*A2+mge3*A3-A4]
        ! y(3) = mgeRx1 * cfluxm2r + mgeRx2 * cfluxs2r + mgeRx3 * cfluxa2r - cfluxr

        ! !microbe K          [mge3*A5+mge4*A6+mge3*A7-A8]
        ! y(4) = mgeKx1 * cfluxm2k + mgeKx2 * cfluxs2k + mgeKx2 * cfluxa2k - cfluxk

        ! !microbe R          [mge1*A1+mge2*A2+mge1*A3-A4]
         y(3) = mgeRx1 * cfluxm2r + mgeRx2 * cfluxs2r + mgeRx1 * cfluxa2r - cfluxr

        ! !microbe K          [mge3*A5+mge4*A6+mge3*A7-A8]
         y(4) = mgeKx1 * cfluxm2k + mgeKx2 * cfluxs2k + mgeKx1 * cfluxa2k - cfluxk

         !physically protected SOM: [Lm*fm+fpr*A4+fpk*A8-A9]
         y(5) = cinputmx * fmx + cfluxr2p + cfluxk2p - cfluxp2a 

         ! chemically protected SOM: [Is*fs+fcr*A4+fck*A8-A10]
         y(6) = cinputsx * fsx + cfluxr2c + cfluxk2c - cfluxc2a 

         !active SOM: [far*A4+fak*A8+A9+A10-A3-A7]
         y(7) = cfluxr2a + cfluxk2a + cfluxp2a + cfluxc2a - cfluxa2r - cfluxa2k
		 ! additional dummy pools
		 y(8) = 0.0
		 y(9) = 0.0
		 y(10)= 0.0
		 
      endif

      ! the new soil carbon model combining MIMICS and MILLENNIAL2
      ! we use two litter pools (m,s) and two microbial pool (r,k) and LWC (pool a), aggregate C (pool p) amd MAOC (pool C)
      ! see documentation on the combined model
      if(kinetics==3) then      
        ! reverse Michaelis-Menten for litter and forward MM for pool 7
        cfluxm2r = xpool(1) * V1x * phx2 * xwater2 * xpool(3)/(K1x + xpool(3))    ! eqn 2 Abramoff2022
        cfluxs2r = xpool(2) * V2x * phx2 * xwater2 * xpool(3)/(K2x + xpool(3))    ! eqn 2 Abramoff2022
        cfluxa2r = xpool(3) * V3x * phx2 * xwater2 * xpool(7)/(K3x + xpool(7))
!        cfluxa2r = xpool(7) * V3x * xwater2 * xpool(3)/(K3x + xpool(3))    ! eqn 2 Abramoff2022 

        cfluxm2k = xpool(1) * W1x * phx3 * xwater2 * xpool(4)/(J1x + xpool(4))    ! eqn 2 Abramoff2022
        cfluxs2k = xpool(2) * W2x * phx3 * xwater2 * xpool(4)/(J2x + xpool(4))    ! eqn 2 Abramoff2022
        cfluxa2k = xpool(4) * W3x * phx3 * xwater2 * xpool(7)/(J3x + xpool(7))
!        cfluxa2k = xpool(7) * W3x * xwater * xpool(4)/(J3x + xpool(4))    ! eqn 2 Abramoff2022
       ! forward Michaelis-Menten
!        cfluxm2r = xpool(3) * V1x * xpool(1)/(K1x + xpool(1))
!        cfluxs2r = xpool(3) * V2x * xpool(2)/(K2x + xpool(2))
!        cfluxa2r = xpool(3) * V3x * xpool(7)/(K3x + xpool(7))

!        cfluxm2k = xpool(4) * W1x * xpool(1)/(J1x + xpool(1))
!        cfluxs2k = xpool(4) * W2x * xpool(2)/(J2x + xpool(2))
!        cfluxa2k = xpool(4) * W3x * xpool(7)/(J3x + xpool(7))
        
        cfluxa   = tvacx    * xwater1 * xpool(7)                           ! eqn 8 Abramoff2022 (leaching)
        cfluxa   = 0.0                                                    ! labile C leaching is done separately
        cfluxr   = tvmicRx  * xpool(3) ** betamicR                        ! eqv. eqn(16) Abramoff2022 
        cfluxk   = tvmicKx  * xpool(4) ** betamicK                        ! eqv. eqn(16) Abramoff2022
!        cfluxp   = tvppoolx * xwater1 * xpool(5)                          ! eqv. eqn (6) abramoff2022 (aggregate breakdown)
        cfluxc2p = tvcpoolx * xwater1 * xpool(6)                          ! eqn(18) Abramoff2022  , all flux to aggregate C      
       
        ! flux to MAOC (pool c)
        cfluxa2c = kadsorpx * phx1 * xwater1 * xpool(7) * (1.0 -  xpool(6)/qmax)   ! eqn(9)  abramoff2022
        cfluxr2c = fr2cx * cfluxr                                                ! p_b*F_bm in eqn(19) Abramoff2022
        cfluxk2c = fk2cx * cfluxk                                                ! p_b*F_bm in eqn(19) Abramoff2022
!        cfluxp2c = (1.0-fp2ax) * cfluxp                                          !(1-p_a)*F_a  in eqn(19) of Abramoff2022 

        ! flux to low weight mass C (pool a)
        cfluxr2a = (1.0-fr2cx) * cfluxr                                          !(1-p_b)*F_bm in eqn(7) Abramoff2022
        cfluxk2a = (1.0-fk2cx) * cfluxk                                          !(1-p_b)*F_bm in eqn(7) Abramoff2022 
!        cfluxp2a = fp2ax * cfluxp                                                ! p_a*F_a  different from Abramoff2022, Aggregate -> active, not litter
!        ! this equation is wrong  (see Wang et al. 2022, their GCB papes, Tables S3 and S5)
!        cfluxc2a = kadsorpx * xpool(6)/qmax                                      ! eqn(12) Abramoff2022
        ! based on Wang et al. (2022)
        cfluxc2a = kdesorpx * xpool(6)/qmax   

!       disaggregation fluxes
        cfluxp2m = tvppoolx * xwater1 * xpool(5)   !metabolic pool
        cfluxp2s = tvppoolx * xwater1 * xpool(8)   !structural litter pool
        cfluxp2c = tvppoolx * xwater1 * xpool(9)   !MAOC  assuming disaggregation of aggregagated MAOC is slower than POC


         y(1) = cinputmx * (1.0-fmx) + cfluxp2m - cfluxm2r - cfluxm2k                       ! same as kinetics=2

         ! structural litter [=Is*(1-fs)-A2-A6]
         y(2) = cinputsx * (1.0-fsx) + cfluxp2s - cfluxs2r - cfluxs2k                       ! same as kinetics=2

         !microbe R          [mge1*A1+mge2*A2+mge3*A3-A4]
         y(3) = mgeRx1 * cfluxm2r + mgeRx2 * cfluxs2r + mgeRx1 * cfluxa2r - cfluxr ! same as kinetics=2

         !microbe K          [mge3*A5+mge4*A6+mge3*A7-A8]
         y(4) = mgeKx1 * cfluxm2k + mgeKx2 * cfluxs2k + mgeKx1 * cfluxa2k - cfluxk ! same as kinetics =2
        
         !Aggregate metabolic C (pool p)
         y(5) = cinputmx * fmx   - cfluxp2m                             

         !MAOC (pool c)
         y(6) = cfluxr2c + cfluxk2c + cfluxp2c + cfluxa2c  - cfluxc2a - cfluxc2p    ! eqn(19) of abramoff2022
                                                                                    ! cfluxa2c<->F_lm (sorption);   cfluxc2a<->F_ld (desorption)
                                                                                    ! cinputsx * fsx<-> no match;   (cfluxr2c + cfluxk2c)<->p_b * F_bm
                                                                                    ! cfluxp2c<->(1-p_a)*F_a;        cfluxc2p<->F_ma
                                                                                    !  
         !LWC
         y(7) = cfluxr2a + cfluxk2a + cfluxc2a - cfluxa - cfluxa2r - cfluxa2k - cfluxa2c              ! eqn(7) Abramoff2022
                                                                                    ! no litter litter input. None <-> F_i
                                                                                    ! cfluxa: F_l (leaching)
                                                                                    ! no depolymeration <->F_pl :: litter input does not enter this pool directly
                                                                                    ! cfluxa2c<->F_lm (adsorption)
                                                                                    ! (cfluxa2r + cfluxa2k)<-> F_lb
                                                                                    ! (cfluxr2a + cfluxk2a)<->(1-p_b)*F_bm,  necromass input 
                                                                                    ! cfluxc2a<->F_ld, (desorption)
                                                                                    ! clfuxp2a: de-aggregation       ! different from Abramoff2022                                                                               
        ! aggregated structural C
        y(8) = cinputsx * fsx - cfluxp2s
        ! aggregated MAOC
        y(9) = cfluxc2p  - cfluxp2c
        ! dummy pool
        y(10) = 0.0
        ! check mass balance
        rsoil = (1.0-mgeRx1) * (cfluxm2r+cfluxa2r) + (1.0-mgeKx1)* (cfluxm2k + cfluxa2k)  &
              + (1.0-mgeRx2) * cfluxs2r            + (1.0-mgeKx2)* cfluxs2k  - cfluxa

!       write(*,101) np,ns, cinputmx+cinputsx,sum(y(1:7)),rsoil, cinputmx+cinputsx-sum(y(1:7))-rsoil
101 format('vmic_c: input, sumdelc rsoil',2(i3,1x),10(f10.6,1x))      
      endif

!      print *, ' @ vmic_c xpool =', xpool(:)
!      print *, ' @ vmic_c y =', y(:)
      
      if(isoc14==1) then
  
         do ip=1,mcpool
            y(ip) = y(ip) - tvc14 * max(0.0,xpool(ip))
         enddo
      endif
      
   end subroutine vmic_c  

! ##############mesc_model.f90###########################   
