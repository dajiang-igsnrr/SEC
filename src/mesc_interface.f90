!> interface between the "mesc_function.f90" and "mesc_model.f90" for doing the following tasks
!! (1) assign parameter values (vmic_param_xscale) or read parameter values from lookup table (functn_global)
!! (2) calling one of the following routines
!!     vmicsoil_c14; vmicsoil_frc1_cpu; vmicsoil_hwsd_cpu
!! (3) within each of three subroutines; we do the following tasks
!! (3a) assign parameters with constant values across different "bgc types"
!! (3b) assign initial pool sizes
!! (3c) compute time-varying parameter values
!! (3d) read in restart file (if) jrestart==1
!! (3d) call model for each of mp and integration over time 
!!
! ##############mesc_interface.f90###########################
!
SUBROUTINE vmic_param_constant(kinetics,micpxdef,micpdef,micparam,zse) 
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_param_xscale),       INTENT(IN)    :: micpxdef 
    TYPE(mic_param_default),      INTENT(IN)    :: micpdef  
    TYPE(mic_parameter),          INTENT(INout) :: micparam
    real(r_2) zse(ms)
    !local variables   
    real(r_2), dimension(:,:), allocatable      :: froot
    real(r_2), dimension(:),   allocatable      :: totroot
    integer    nopt,npft,np,ns,kinetics
    real(r_2)  depths1,depths2,krootx


      allocate(froot(mpft,ms))
      allocate(totroot(mpft))

      do np=1,mp
!         print *, 'np pft', np,micparam%pft(np)
          nopt=micparam%bgctype(np)
          do ns=1,ms
            micparam%Q1(np,ns)=micpdef%Q1
            micparam%Q2(np,ns)=micpdef%Q2
            micparam%fm(np,ns)=micpdef%fm * micpxdef%xfm(nopt)
            micparam%fs(np,ns)=micpdef%fs * micpxdef%xfs(nopt)
         enddo  !ns
      enddo     ! np
      
      depths1=0.0;depths2=0.0
      do ns=1,ms
          depths2 = depths2 + zse(ns)
          do npft=1,mpft
              krootx = micpdef%rootbeta * micpxdef%xrootbeta(npft)
              froot(npft,ns) = (1.0/krootx) *( exp(-krootx*depths1)-exp(-krootx*depths2))
          enddo  !npft
          depths1=depths2
      enddo   !ns

   
      do npft=1,mpft
         totroot(npft) =sum(froot(npft,1:ms))
      enddo

      ! !normalizing
      do ns=1,ms
         do npft=1,mpft
            froot(npft,ns) = froot(npft,ns)/totroot(npft)
         enddo
      enddo

      ! calculate mp by ms all parameter values
      do np=1, mp
         npft=min(mpft,max(1,micparam%pft(np)))
         nopt=micparam%bgctype(np)
         do ns=1,ms
            micparam%sdepth(np,ns)   = zse(ns)
            micparam%fracroot(np,ns) = froot(npft,ns)
         enddo !"ns"
          micparam%diffsocx(np) = micpxdef%xdiffsoc(nopt) * micpdef%diffsoc  !"diffsoc" from mic_constant
      enddo    ! "np=1,mp"

      if(diag==1) then
         print *, micparam%fracroot(outp,:) 
         print *, micparam%sdepth(outp,:)
         print *, micparam%diffsocx(outp)
      endif
      
      ! the following parameters are specific to kinetics3
      if(kinetics==3) then
         do np=1,mp
            do ns=1,ms
               nopt=micparam%bgctype(np)
               micparam%kadsorp(np,ns)  = micpdef%kadsorpx
               micparam%kdesorp(np,ns)  = micparam%kadsorp(np,ns) /(micpdef%kbax * micpxdef%xkba(nopt))
               micparam%fp2a(np,ns)     = micpdef%fp2ax     * micpxdef%xfp2ax(nopt)
               micparam%tvcpool(np,ns)  = micpdef%tvcpoolx  * micpxdef%xtvc(nopt)
               micparam%tvppool(np,ns)  = micpdef%tvppoolx  * micpxdef%xtvp(nopt)
               micparam%tvac(np,ns)     = micpdef%tvacx     * micpxdef%xtvac(nopt)
               micparam%qmaxcoeff(np,ns)= micpdef%qmaxcoeff * micpxdef%xqmaxcoeff(nopt)
            enddo  !ns
         enddo  !np
      endif ! kinetics


    deallocate(froot)
    deallocate(totroot)    
END SUBROUTINE vmic_param_constant  

subroutine vmic_param_time(kinetics,micpxdef,micpdef,micparam,micinput,micnpool)
    ! time-dependent model parameters, called every time step if the forcing, such air temperature
    ! varies every time step
    ! otherwise only called at the start the integration	
    use mic_constant 
    use mic_variable
    implicit none
    TYPE(mic_param_xscale),       INTENT(IN)      :: micpxdef      
    TYPE(mic_param_default),      INTENT(IN)      :: micpdef  
    TYPE(mic_parameter),          INTENT(INout)   :: micparam
    TYPE(mic_input),              INTENT(INout)   :: micinput
    TYPE(mic_npool),              INTENT(INOUT)   :: micnpool

    integer  kinetics
      ! compute fractions
      call bgc_fractions(micpxdef,micpdef,micparam,micinput)
      ! compute microbial growth efficiency
      call mget(micpdef,micparam,micinput,micnpool)
      ! compute microbial turnover rates
      call turnovert(kinetics,micpxdef,micpdef,micparam,micinput)
      if(kinetics/=3) call Desorpt(micpxdef,micparam,micinput) 
      call Vmaxt(micpxdef,micpdef,micparam,micinput)
      call Kmt(micpxdef,micpdef,micparam,micinput)
  
end subroutine vmic_param_time


subroutine vmic_param_time_single(kinetics,micpxdef,micpdef,micparam,micinput,micnpool,np)
    ! time-dependent model parameters, called every time step if the forcing, such air temperature
    ! varies every time step
    ! otherwise only called at the start the integration	
    use mic_constant 
    use mic_variable
    implicit none
    TYPE(mic_param_xscale),       INTENT(IN)      :: micpxdef      
    TYPE(mic_param_default),      INTENT(IN)      :: micpdef  
    TYPE(mic_parameter),          INTENT(INout)   :: micparam
    TYPE(mic_input),              INTENT(INout)   :: micinput
    TYPE(mic_npool),              INTENT(INOUT)   :: micnpool
    integer,                      INTENT(IN)      :: np

    integer  kinetics
      ! compute fractions
      call bgc_fractions_single(micpxdef,micpdef,micparam,micinput,np)
      ! compute microbial growth efficiency
      call mget_single(micpdef,micparam,micinput,micnpool,np)
      ! compute microbial turnover rates
      call turnovert_single(kinetics,micpxdef,micpdef,micparam,micinput,np)
      if(kinetics/=3) call Desorpt_single(micpxdef,micparam,micinput,np) 
      call Vmaxt_single(micpxdef,micpdef,micparam,micinput,np)
      call Kmt_single(micpxdef,micpdef,micparam,micinput,np)
  
end subroutine vmic_param_time_single


subroutine vmic_init(miccpool,micnpool)
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_cpool),              INTENT(INOUT)   :: miccpool
    TYPE(mic_npool),              INTENT(INOUT)   :: micnpool
    integer ip
    real(r_2), dimension(:), allocatable    :: cpooldef

      allocate(cpooldef(mcpool))
!	  print *, 'calling vmic_init'

      cpooldef(1) = 16.5*0.1;     cpooldef(2) = 16.5*0.1
      cpooldef(3) = 16.5*0.025;   cpooldef(4) = 16.5*0.025
      cpooldef(5) = 16.5*0.1125;  cpooldef(6) = 16.5*0.375;  cpooldef(7) = 16.5*0.2625
	  cpooldef(8) = 0.0;          cpooldef(9) = 0.0;         cpooldef(10)= 0.0

      do ip=1,mcpool
         miccpool%cpool(:,:,ip) = cpooldef(ip)
      enddo
!    print *, 'at np=1 ns=1 cpool', miccpool%cpool(1,1,1:mcpool)
    deallocate(cpooldef)    
end subroutine vmic_init


subroutine vmic_param_xscale(xopt,bgcopt,jmodel,micpxdef)
    use mic_constant 
    use mic_variable
    implicit none
    TYPE(mic_param_xscale),  INTENT(INOUT)   :: micpxdef
    integer bgcopt,jmodel
    real*8, dimension(16)                    :: xopt
!    real(r_2), dimension(17)                 :: xrootcable
!    real(r_2), dimension(18)                 :: xrootorchidee   
!    data xrootcable/1.43,0.94,1.43,1.04,0.77,0.85,0.62,1.77,0.94,0.94,1.43,0.94,1.04,0.53,1.00,1.00,1.00/
!    data xrootorchidee/0.94,0.94,1.04,1.04,1.04,1.43,1.43,1.43,0.85,0.62,0.94,0.94,0.85,0.85,0.85,0.85,0.85,0.85/
    integer i

! the order is fixed. 
!  1: xav:      scaling factor for V                            [1]              (0-30)               8.0e-6
!  2: xak:      scaling factor for K                            [1]              (0-30)               10.0
!  3: xfm:      scaling factor for fm                           [1]              (0.1-5.0)            0.05
!  4: xfs:      scaling factor for fs                           [1]              (0.1-5.0)            0.05
!  5: xtvmic:   scaling factor for tvmicR (0-10)                [1]              (0.1,10)             sqrt(NPPP)
!  5: xtvmic :  scaling factor for tvmicK (0-10)                [1]              =xtvmicR             sqrt(NPPP)
!  6: xtvp:     scaling factor for tvppool(0-10)                [1]            (0.1,10)               1/25  year-1    ! rate of
!  disaggregation
!  7: xtvc:     scaling factor for tvcpool(0-10)                [1]            (0.1,10)               1/100 year-1    ! rate of MAOC
!  breakdown
!  8: xtvac:    scaling factor for tvac   (0-10)                [1]            (0.1,10)               1/2.0 year-1    ! leaching
!  rate
!  9: xkba:     scaling factor for kba    (0.2-5)               [1]            (0.5,10)               2.0             ! ratio of
!  adsorption/desoprtion
! 10: xqmaxcoeff:coefficient of Qmax on clay+silt (0.4-0.8)     [1]            (0.5,5.0)              0.6
! 11: xdiffsoc:  SOC diffusion/bioturbation rate                [1]            (0.1,10.0)             (1.0/24.0)* 2.74e-3 (1/day)
! 12: xNPP:      carbon input                                   [1]            (0.5,2.0)              NPP
! 13: xrootbeta: scaling for depth-dependent of root C input    [1]            (0.5,5.0)              2.0
! 14: xvmaxbeta: scaling for depth-dependent of vmax            [1]            (0.5,5.0)              2.0

     ! assign the default values
     ! this shoudl be replaced by a parameter lookup tables for gloabl simulations
      micpxdef%xav       = 1.0
      micpxdef%xak       = 1.0
      micpxdef%xfm       = 1.0
      micpxdef%xfs       = 1.0
      micpxdef%xtvmic    = 1.0
      micpxdef%xtvp      = 1.0
      micpxdef%xtvc      = 1.0   ! unstop the backflow from MAOC to aggregate pool
      micpxdef%xtvac     = 1.0
      micpxdef%xkba      = 1.0
      micpxdef%xqmaxcoeff= 1.0
      micpxdef%xdiffsoc  = 1.0
      micpxdef%xNPP      = 1.0   
      micpxdef%xrootbeta = 1.0
      micpxdef%xvmaxbeta = 1.0
      
      micpxdef%xfp2ax    = 1.0
      micpxdef%xbeta     = 1.0
      micpxdef%xdesorp   = 1.0
      
      do i=1,mpft
         if(jmodel==1) then
            micpxdef%xrootbeta(i) = xrootcable(i) 
         endif
         if(jmodel==2) then
            micpxdef%xrootbeta(i) = xrootorchidee(i)
         endif
      enddo         

      ! assign the values to the optimized parameters
      
      micpxdef%xav(bgcopt)        = xopt(1)
      micpxdef%xak(bgcopt)        = xopt(2)
      micpxdef%xfm(bgcopt)        = xopt(3)
      micpxdef%xfs(bgcopt)        = xopt(4)
      micpxdef%xtvmic(bgcopt)     = xopt(5)
      micpxdef%xtvp(bgcopt)       = xopt(6)
      micpxdef%xtvc(bgcopt)       = xopt(7)
      micpxdef%xtvac(bgcopt)      = xopt(8)
      micpxdef%xkba(bgcopt)       = xopt(9)
      micpxdef%xqmaxcoeff(bgcopt) = xopt(10)
      micpxdef%xdiffsoc(bgcopt)   = xopt(11)
     ! NPP was from CABLE/ORCHIDEE  
      micpxdef%xnpp(:)            = xopt(12)
     ! "rootbeta" was assigned above based on mean profile for each PFT in HWSD_SOC
     ! micpxdef%xrootbeta(pftopt)  = xopt(13)
      micpxdef%xvmaxbeta(bgcopt)  = xopt(14) 

end subroutine vmic_param_xscale


subroutine variable_time(year,doy,micglobal,micinput,micnpool)
    use mic_constant 
    use mic_variable
    implicit none
    integer year,doy
    TYPE(mic_global_input),  INTENT(INout)   :: micglobal
    TYPE(mic_input),         INTENT(INout)   :: micinput
    TYPE(mic_npool),         INTENT(INOUT)   :: micnpool
    integer np,ns

    
!        print *, 'calling global2np- ntime', ntime
     do np=1,mp
        micinput%fcnpp(np)      = max(0.0,micglobal%npp(np))              !gc/m2/year
        micinput%Dleaf(np)      = (micglobal%dleaf(np,doy)/24.0)*delt     !gc/m2/delt
        micinput%Droot(np)      = (micglobal%droot(np,doy)/24.0)*delt     !gc/m2/delt
        micinput%Dwood(np)      = (micglobal%dwood(np,doy)/24.0)*delt     !gc/m2/delt

        do ns=1,ms
           micinput%tavg(np,ns)     = micglobal%tsoil(np,ns,doy)  ! average temperature in deg C
           micinput%wavg(np,ns)     = micglobal%moist(np,ns,doy)  ! average soil water content mm3/mm3
           micinput%matpot(np,ns)   = micglobal%matpot(np,ns,doy)
           micinput%clay(np,ns)     = micglobal%clay(np)     ! clay content (fraction)
           micinput%silt(np,ns)     = micglobal%silt(np)     ! silt content (fraction)
           micinput%ph(np,ns)       = micglobal%ph(np)
           micinput%porosity(np,ns) = micglobal%poros(np)    ! porosity mm3/mm3
           micinput%bulkd(np,ns)    = micglobal%bulkd(np)
           micnpool%mineralN(np,ns) = 0.1                    ! g N /kg soil
        enddo !"ns"    
     enddo !"np"

end subroutine variable_time

subroutine variable_time_single(year,doy,micglobal,micinput,micnpool,np)
    use mic_constant 
    use mic_variable
    implicit none
    integer year,doy
    TYPE(mic_global_input),  INTENT(IN)   :: micglobal
    TYPE(mic_input),         INTENT(INout)   :: micinput
    TYPE(mic_npool),         INTENT(INOUT)   :: micnpool
    integer, INTENT(IN) :: np
    integer ns

    
!        print *, 'calling global2np- ntime', ntime
   micinput%fcnpp(np)      = max(0.0,micglobal%npp(np))              !gc/m2/year
   micinput%Dleaf(np)      = (micglobal%dleaf(np,doy)/24.0)*delt     !gc/m2/delt
   micinput%Droot(np)      = (micglobal%droot(np,doy)/24.0)*delt     !gc/m2/delt
   micinput%Dwood(np)      = (micglobal%dwood(np,doy)/24.0)*delt     !gc/m2/delt

   do ns=1,ms
      micinput%tavg(np,ns)     = micglobal%tsoil(np,ns,doy)  ! average temperature in deg C
      micinput%wavg(np,ns)     = micglobal%moist(np,ns,doy)  ! average soil water content mm3/mm3
      micinput%matpot(np,ns)   = micglobal%matpot(np,ns,doy)
      micinput%clay(np,ns)     = micglobal%clay(np)          ! clay content (fraction)
      micinput%silt(np,ns)     = micglobal%silt(np)          ! silt content (fraction)
      micinput%ph(np,ns)       = micglobal%ph(np)
      micinput%porosity(np,ns) = micglobal%poros(np)         ! porosity mm3/mm3
      micinput%bulkd(np,ns)    = micglobal%bulkd(np)
      micnpool%mineralN(np,ns) = 0.1                         ! g N /kg soil
   enddo !"ns"    

end subroutine variable_time_single

    subroutine vmicsoil_c14(jrestart,frestart_in,frestart_out,foutput,kinetics,isoc14,ifsoc14,bgcopt,nyeqpool, &
                    zse,micpxdef,micpdef,micparam,micinput,micglobal,miccpool,micnpool,micoutput)
    use mic_constant 
    use mic_variable
   !  use omp_lib
    implicit none
    TYPE(mic_param_xscale),  INTENT(INOUT)   :: micpxdef      
    TYPE(mic_param_default), INTENT(IN)      :: micpdef  
    TYPE(mic_parameter),     INTENT(INout)   :: micparam
    TYPE(mic_input),         INTENT(INout)   :: micinput
    TYPE(mic_global_input),  INTENT(INout)   :: micglobal
    TYPE(mic_cpool),         INTENT(INOUT)   :: miccpool
    TYPE(mic_npool),         INTENT(INOUT)   :: micnpool
    TYPE(mic_output),        INTENT(INout)   :: micoutput
    real(r_2) zse(ms)
    integer ifsoc14,isoc14,kinetics,bgcopt,nyeqpool

    ! local variables
    real(r_2),    dimension(mcpool)    :: xpool0,xpool1
    real(r_2),    dimension(ms)        :: ypooli,ypoole,fluxsoc
    real(r_2),    dimension(ms)        :: cfluxa
    
!    integer       ndelt,n1,n2,i,j,year,ip,np,ns,ny,nyrun
    integer       ndelt,i,j,year,ip,np,ns,ny,nyrun
    real(r_2)     timex,delty,fluxdocsx,diffsocxx

    integer    jrestart
    character*140 frestart_in,frestart_out,foutput
    real(r_2)  cpool0, cpool1, totcinput  

   ! local variables
   real(r_2)                      deltD !,tot0,tot1,totflux
   real(r_2), dimension(ms)   :: xzse
   real(r_2), dimension(ms+1) :: sdepthx
   real(r_2)                     coeffA, coeffB
   real(r_2), dimension(ms)   :: at,bt,ct,rt
   real(r_2), dimension(ms)   :: xpool
   real(r_2)  cleachloss


  !  allocate(xzse(ms))
  !  allocate(sdepthx(ms+1))
  !  allocate(at(ms),bt(ms),ct(ms),rt(ms))
  !  allocate(xpool(ms))
    
      call vmic_param_constant(kinetics,micpxdef,micpdef,micparam,zse) 
      call vmic_init(miccpool,micnpool)
      call vmic_param_time(kinetics,micpxdef,micpdef,micparam,micinput,micnpool)  
      
    !  print *, 'initial pool size np=1 ns=1', miccpool%cpool(1,1,:)
    !  print *, 'xav=', bgcopt,micpxdef%xav(:)
      
      if(jrestart==1) call vmic_restart_read(miccpool,micnpool,frestart_in)      
  
      ndelt   = int(24*365/delt) ! number of time step per year in "delt" unit

! "data copyin" : all data accessed by GPU
! "create": intermediate variables used by GPU
! "copyout" copy data out from GPU
! "private": every cell-dependent variables used in paralelling computing  

!$acc data copyin(micpdef,micparam,miccpool,micinput,micoutput,  &
!$acc nyrun,bgcopt,ndelt,zse,kinetics,nyeqpool,isoc14)      &
!$acc create(delty,timex,fluxsoc,year,ny,i,ns,np,ip,xpool0,xpool1,ypooli,ypoole,diffsocxx,cfluxa, &
!$acc j,deltD,xzse,sdepthx,coeffA,coeffB,at,bt,ct,rt,xpool,cleachloss)              &
!$acc copyout(miccpool%cpool,micoutput%fluxcinput,micoutput%fluxrsoil,micoutput%fluxcleach)
!$acc PARALLEL LOOP                                                      &
!$acc private(delty,timex,fluxsoc,year,ny,i,ns,ip,np,xpool0,xpool1,ypooli,ypoole,diffsocxx,cfluxa,&
!$acc j,deltD,xzse,sdepthx,coeffA,coeffB,at,bt,ct,rt,xpool,cleachloss)

      do np=1,mp
      
      if(micparam%bgctype(np)==bgcopt) then
         if (ifsoc14 == 1) then
             nyrun = micparam%nyc14obs(np) - 1940 + nyeqpool !! how many years to run to get equilibrium 
         else
             nyrun = nyeqpool
         endif
  
    !     print *,'np pft npp anpp bnpp = ',np,micparam%pft(np),micinput%fcnpp(np), micinput%dleaf(np)*365.0*24.0, micinput%droot(np)*365.0*24.0

         do year=1,nyrun 
            ny = year-nyrun
            micoutput%fluxcinput(np)=0.0; micoutput%fluxrsoil(np) = 0.0; micoutput%fluxcleach(np)= 0.0    ! yearly fluxes
             
            do i=1,365          

               ! for each soil layer
               ! sum last all C pools of all layers for compute the soil respiration = input - sum(delCpool)
               ! before leaching is computed
                cpool0 =0.0; cpool1 =0.0; totcinput = 0.0            
               do ns=1,ms
                 ! micinput%cinputm(np,ns)+micinput%cinputs(np,ns) in mg C/cm3/delt
                  totcinput =totcinput + (micinput%cinputm(np,ns)+micinput%cinputs(np,ns)) *1000.0 * zse(ns)   ! convert to g C/m2/delt/zse

                  do ip=1,mcpool
                     xpool0(ip) = miccpool%cpool(np,ns,ip)
                     cpool0     = cpool0  + xpool0(ip) * zse(ns) * 1000.0  ! 1000 for mg C/cm3 to g C/m2/zse                     
                  enddo

                 ! here the integration step is "delty" in rk4 and "ndelt" is number of "delt (hour) per year 
                  timex=real(i*delt)
                  delty = real(ndelt)/(365.0*delt)  ! time step in rk4 in "24 * delt (or daily)", all C input are in " per delt"
                  call rk4modelx(timex,delty,ny,isoc14,np,ns,kinetics,micpdef,micparam,micinput,xpool0,xpool1)  

                  do ip=1,mcpool
                     miccpool%cpool(np,ns,ip) = max(xpool1(ip),1.0e-8)
                     cpool1 = cpool1 + miccpool%cpool(np,ns,ip) * zse(ns) * 1000.0  ! 1000 for mg C/cm3 to g C/m2/zse                        
                  enddo
              
               enddo    ! "ns"

               micoutput%fluxcinput(np)= micoutput%fluxcinput(np) + totcinput * real(delty)        
               micoutput%fluxrsoil(np) = micoutput%fluxrsoil(np)  + totcinput * real(delty) + (cpool1 - cpool0)  

                if(diag==1) then  
                   print *, 'year day site np1', year, i, outp,micparam%diffsocx(outp)
                    do ns=1,ms
                       print *, ns, miccpool%cpool(outp,ns,:) 
                    enddo  
                endif
  
               do ip=1,mcpool
                  do ns=1,ms
                     ypooli(ns) = miccpool%cpool(np,ns,ip)      ! in mg c/cm3
                  enddo  !"ns"
            
                  fluxsoc(:) = 0.0  ! This flux is added in "modelx"
                  diffsocxx= micparam%diffsocx(np)

            
                 !Move bioturb here to work around memory auto allocation failure
                 !call bioturb(int(delty/delty),ms,zse,delty,diffsocxx,fluxsoc,ypooli,ypoole)  ! only do every 24*delt
                 !subroutine bioturb(ndelt,ms,zse,delt,diffsocxx,fluxsoc,xpooli,xpoole)
                  
                  sdepthx(1) = 0.0          ! depth of a layer from the top (x_0.5=0.0 eg soil surface)
                  do j=2,ms+1
                     sdepthx(j) = sdepthx(j-1) + zse(j-1)*100.0     ! depth of the bottom of each layer (eg x_j+0.5)
                                                                    !*100 to convert from m to cm
                  enddo
             
                  do j=1,ms
                     xzse(j) = 0.5 * (sdepthx(j) + sdepthx(j+1))    ! depth of midpoint of a layer j  (x_j)
                  enddo
             
                  deltD = diffsocxx * delty
                  xpool = ypooli
               
                  !do i=1,1 ( int(delty/delty) == 1 )
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
                  !enddo
                  ypoole = xpool
                  
                  !!! end bioturb
            
                  do ns=1,ms
                     miccpool%cpool(np,ns,ip) = ypoole(ns)
                  enddo
               enddo ! "ip=1,mcpool"
   
               ! computing daily leaching loss from bottom-layer LWMC 
               cleachloss = micparam%tvac(np,ms) * sqrt(micinput%wavg(np,ms)/micinput%porosity(np,ms)) *  miccpool%cpool(np,ms,7)  * 24.0
               cleachloss = max(0.0,min(cleachloss,miccpool%cpool(np,ms,7)))
               micoutput%fluxcleach(np) = micoutput%fluxcleach(np) + cleachloss
               miccpool%cpool(np,ms,7)  = miccpool%cpool(np,ms,7)  - cleachloss               
               
            enddo   ! "day"      
   
         enddo !"year"

      endif   !bgctype(np)=bgcopt
   enddo !"mp"                                                                                                       

!$ACC END PARALLEL
!$ACC END DATA 
 
  
    miccpool%cpooleq(:,:,:) = miccpool%cpool(:,:,:)
     
   !  call vmic_output_write(foutput,micinput,micoutput)
   !  call vmic_restart_write(frestart_out,miccpool,micnpool)

   ! deallocate(xzse)
   ! deallocate(sdepthx)
   ! deallocate(at,bt,ct,rt)
   ! deallocate(xpool)

    end subroutine vmicsoil_c14

    SUBROUTINE vmicsoil_frc1_cpu(jrestart,frestart_in,frestart_out,foutput,kinetics,isoc14,ifsoc14,bgcopt,nyeqpool, &
                        zse,micpxdef,micpdef,micparam,micinput,micglobal,miccpool,micnpool,micoutput)
    use mic_constant 
    use mic_variable
   !  use omp_lib
    implicit none
    TYPE(mic_param_xscale),  INTENT(INOUT)   :: micpxdef      
    TYPE(mic_param_default), INTENT(IN)      :: micpdef  
    TYPE(mic_parameter),     INTENT(INout)   :: micparam
    TYPE(mic_input),         INTENT(INout)   :: micinput
    TYPE(mic_global_input),  INTENT(INout)   :: micglobal
    TYPE(mic_cpool),         INTENT(INOUT)   :: miccpool
    TYPE(mic_npool),         INTENT(INOUT)   :: micnpool
    TYPE(mic_output),        INTENT(INout)   :: micoutput
    real(r_2) zse(ms)
    integer ifsoc14,isoc14,kinetics,bgcopt,nyeqpool

    ! local variables
    real(r_2),    dimension(mcpool)    :: xpool0,xpool1
    real(r_2),    dimension(ms)        :: ypooli,ypoole,fluxsoc
    real(r_2),    dimension(ms)        :: cfluxa
    
!    integer       ndelt,n1,n2,i,j,year,ip,np,ns,ny,nyrun
    integer       ndelt,i,j,year,ip,np,ns,ny,nyrun
    real(r_2)     timex,delty,fluxdocsx,diffsocxx

    integer    jrestart
    character*140 frestart_in,frestart_out,foutput
    real(r_2)  cpool0, cpool1, totcinput  

   ! local variables
   real(r_2)                      deltD !,tot0,tot1,totflux
   real(r_2), dimension(ms)    :: xzse
   real(r_2), dimension(ms+1)  :: sdepthx
   real(r_2)                      coeffA, coeffB
   real(r_2), dimension(ms)    :: at,bt,ct,rt
   real(r_2), dimension(ms)    :: xpool
   real(r_2)  cleachloss
   real(r_2)  depthx1,depthx2

      
      call vmic_param_constant(kinetics,micpxdef,micpdef,micparam,zse) 
      call vmic_init(miccpool,micnpool)
      call vmic_param_time(kinetics,micpxdef,micpdef,micparam,micinput,micnpool)  
      
      print *, 'initial pool size np=1 ns=1', miccpool%cpool(1,1,:)
      print *, 'xav=', bgcopt,micpxdef%xav(:)
      
      if(jrestart==1) call vmic_restart_read(miccpool,micnpool,frestart_in)      
  
      ndelt   = int(24*365/delt) ! number of time step per year in "delt" unit

!$OMP PARALLEL DEFAULT(NONE) SHARED (micparam,micpxdef,micnpool,micinput,micglobal,miccpool,micoutput,micpdef,&
!$OMP kinetics,isoc14,ifsoc14,nyeqpool,bgcopt,ndelt,zse,mp,ms) &
!$OMP PRIVATE (np,nyrun,ny,year,i,timex,delty, &
!$OMP ns,ip,xpool0,xpool1,fluxsoc,diffsocxx,ypooli,ypoole,cpool0,cpool1,totcinput,depthx1,depthx2)
!$OMP DO

      do np=1,mp
      
      if(micparam%bgctype(np)==bgcopt) then
         if (ifsoc14 == 1) then
             nyrun = micparam%nyc14obs(np) - 1940 + nyeqpool !! how many years to run to get equilibrium 
         else
             nyrun = nyeqpool
         endif
  
    !     print *,'np pft npp anpp bnpp = ',np,micparam%pft(np),micinput%fcnpp(np), micinput%dleaf(np)*365.0*24.0, micinput%droot(np)*365.0*24.0

         do year=1,nyrun 
            ny = year-nyrun
            micoutput%fluxcinput(np)=0.0; micoutput%fluxrsoil(np) = 0.0; micoutput%fluxcleach(np)= 0.0    ! yearly fluxes
             
            do i=1,365          

               ! for each soil layer
               ! sum last all C pools of all layers for compute the soil respiration = input - sum(delCpool)
               ! before leaching is computed
               cpool0 =0.0; cpool1 =0.0; totcinput = 0.0; depthx1=0.0; depthx2=0.0            
               do ns=1,ms
                  ! only compute soil carbon pool dynamics with the depth falls within the sampling depth btw top and bot
                  depthx2=depthx1+zse(ns)
                !  if(depthx1 <= real(micparam%bot(np))*0.01 .and. depthx2 >= real(micparam%top(np))*0.01 ) then
                     ! micinput%cinputm(np,ns)+micinput%cinputs(np,ns) in mg C/cm3/delt
                     totcinput =totcinput + (micinput%cinputm(np,ns)+micinput%cinputs(np,ns)) *1000.0 * zse(ns)   ! convert to g C/m2/delt/zse

                     do ip=1,mcpool
                        xpool0(ip) = miccpool%cpool(np,ns,ip)
                        cpool0     = cpool0  + xpool0(ip) * zse(ns) * 1000.0  ! 1000 for mg C/cm3 to g C/m2/zse                     
                     enddo

                     ! here the integration step is "delty" in rk4 and "ndelt" is number of "delt (hour) per year 
                     timex=real(i*delt)
                     delty = real(ndelt)/(365.0*delt)  ! time step in rk4 in "24 * delt (or daily)", all C input are in " per delt"
                     call rk4modelx(timex,delty,ny,isoc14,np,ns,kinetics,micpdef,micparam,micinput,xpool0,xpool1)  

                     do ip=1,mcpool
                        miccpool%cpool(np,ns,ip) = max(xpool1(ip),1.0e-8)
                        cpool1 = cpool1 + miccpool%cpool(np,ns,ip) * zse(ns) * 1000.0  ! 1000 for mg C/cm3 to g C/m2/zse                        
                     enddo
                !  endif
                  depthx1=depthx2
               enddo    ! "ns"

               micoutput%fluxcinput(np)= micoutput%fluxcinput(np) + totcinput * real(delty)        
               micoutput%fluxrsoil(np) = micoutput%fluxrsoil(np)  + totcinput * real(delty) + (cpool1 - cpool0)  

               if(diag==1) then  
                  print *, 'year day site np1', year, i, outp,micparam%diffsocx(outp)
                  do ns=1,ms
                     print *, ns, miccpool%cpool(outp,ns,:) 
                  enddo  
               endif
               
            enddo   ! "day"      
   
         enddo !"year"

      endif   !pft(np) = pftopt
   enddo !"mp"                                                                                                       

!$OMP END DO
!$OMP END PARALLEL	
 
  
    miccpool%cpooleq(:,:,:) = miccpool%cpool(:,:,:)
     
   !  call vmic_output_write(foutput,micinput,micoutput)
   !  call vmic_restart_write(frestart_out,miccpool,micnpool)

    end SUBROUTINE vmicsoil_frc1_cpu

subroutine vmicsoil_hwsd_cpu(jrestart,frestart_in,frestart_out,foutput,kinetics,isoc14,bgcopt,nyeqpool, &
                    zse,micpxdef,micpdef,micparam,micinput,micglobal,miccpool,micnpool,micoutput)
    use mic_constant 
    use mic_variable
   !  use omp_lib
    implicit none
    TYPE(mic_param_xscale),  INTENT(INOUT)   :: micpxdef      
    TYPE(mic_param_default), INTENT(IN)      :: micpdef  
    TYPE(mic_parameter),     INTENT(INout)   :: micparam
    TYPE(mic_input),         INTENT(INout)   :: micinput
    TYPE(mic_global_input),  INTENT(INout)   :: micglobal
    TYPE(mic_cpool),         INTENT(INOUT)   :: miccpool
    TYPE(mic_npool),         INTENT(INOUT)   :: micnpool
    TYPE(mic_output),        INTENT(INout)   :: micoutput
    real(r_2) zse(ms)
    integer jrestart,isoc14,kinetics,bgcopt,nyeqpool

    ! local variables
    real(r_2),    dimension(:), allocatable  :: xpool0,xpool1
    real(r_2),    dimension(:), allocatable  :: ypooli,ypoole,fluxsoc,cfluxa

 !   real(r_2),    dimension(mcpool)  :: xpool0,xpool1
 !   real(r_2),    dimension(ms)      :: ypooli,ypoole,fluxsoc,cfluxa
    
    integer       ndelt,i,j,year,ip,np,ns,ny
    integer       nyrun,ip5
    real(r_2)     timex,delty,fluxdocsx,diffsocxx
    
    character*140  frestart_in,frestart_out,foutput
    real(r_2)      cpool0, cpool1, totcinput  
    integer       station_count, station_index
    integer, dimension(:), allocatable :: stations_used    

       allocate(xpool0(mcpool),xpool1(mcpool))
       allocate(ypooli(ms),ypoole(ms),fluxsoc(ms),cfluxa(ms))

    !   print *, 'calling vmic_param_constant'
       call vmic_param_constant(kinetics,micpxdef,micpdef,micparam,zse) 
       
    !   print *, 'calling vmic_init'      
       call vmic_init(miccpool,micnpool)
      
       if(jrestart==1) call vmic_restart_read(miccpool,micnpool,frestart_in)      
  
       ndelt   = int(24*365/delt) ! number of time step per year in "delt" unit

   !check which stations to calculate
   station_count = 0
   allocate(stations_used(mp))
   do station_index=1,mp
      if (micparam%bgctype(station_index)==bgcopt .and. micglobal%area(station_index)>0.0) then
         stations_used(station_count+1) = station_index
         station_count = station_count + 1
      endif   !bgctype(np) = bgcopt
   enddo
   
!$OMP PARALLEL DEFAULT(NONE) SHARED (micparam,micpxdef,micnpool,micinput,micglobal,miccpool,micoutput,micpdef,&
!$OMP kinetics,isoc14,nyeqpool,bgcopt,ndelt,zse,mp,ms,ny,i,year,stations_used) &
!$OMP PRIVATE (np,timex,delty,ns,ip,station_index,&
!$OMP xpool0,xpool1,fluxsoc,diffsocxx,ypooli,ypoole,cpool0,cpool1,totcinput,cfluxa) &
!$OMP FIRSTPRIVATE (ntime,station_count)
!!$OMP REDUCTION (+:data_count,data_used)  &,
!$OMP DO   

   do station_index=1,station_count
      np=stations_used(station_index)


      do year=1,nyeqpool
         ny = year-nyeqpool

         micoutput%fluxcinput(np)=0.0; micoutput%fluxrsoil(np) = 0.0; micoutput%fluxcleach(np)= 0.0    ! yearly fluxes
            
         do i=1,ntime
            call variable_time_single(year,i,micglobal,micinput,micnpool,np)
         ! calculate parameter values that depend on soil temperature or moisture (varying with time)
            call vmic_param_time_single(kinetics,micpxdef,micpdef,micparam,micinput,micnpool,np)

               ! for each soil layer
               ! sum last all C pools of all layers for compute the soil respiration = input - sum(delCpool)
               ! before leaching is computed
                cpool0 =0.0; cpool1 =0.0; totcinput = 0.0            
               do ns=1,ms
                 ! micinput%cinputm(np,ns)+micinput%cinputs(np,ns) in mg C/cm3/delt
                  totcinput =totcinput + (micinput%cinputm(np,ns)+micinput%cinputs(np,ns)) *1000.0 * zse(ns)   ! convert to g C/m2/delt/zse

                  do ip=1,mcpool
                     xpool0(ip) = miccpool%cpool(np,ns,ip)
                     cpool0     = cpool0  + xpool0(ip) * zse(ns) * 1000.0  ! 1000 for mg C/cm3 to g C/m2/zse                     
                  enddo

                 ! here the integration step is "delty" in rk4 and "ndelt" is number of "delt (hour) per year 
                  timex=real(i*delt)
                  delty = real(ndelt)/(365.0*delt)  ! time step in rk4 in "24 * delt (or daily)", all C input are in " per delt"
                  call rk4modelx(timex,delty,ny,isoc14,np,ns,kinetics,micpdef,micparam,micinput,xpool0,xpool1)  

                  do ip=1,mcpool
                     miccpool%cpool(np,ns,ip) = max(xpool1(ip),1.0e-8)
                     cpool1 = cpool1 + miccpool%cpool(np,ns,ip) * zse(ns) * 1000.0  ! 1000 for mg C/cm3 to g C/m2/zse                        
                  enddo

                ! for checking mass balance                   
        !          write(*,101) np,ns, micinput%cinputm(np,ns)+micinput%cinputs(np,ns),sum(xpool1(1:7)-xpool0(1:7))/real(delty), &
        !                              micinput%cinputm(np,ns)+micinput%cinputs(np,ns)-sum(xpool1(1:7)-xpool0(1:7))/real(delty)
101 format('vmicsoil input sumdelC rsoil',2(i3,1x),3(f10.6,1x))  
              
               enddo    ! "ns"

               micoutput%fluxcinput(np)= micoutput%fluxcinput(np) + totcinput * real(delty)        
               micoutput%fluxrsoil(np) = micoutput%fluxrsoil(np)  + totcinput * real(delty) + (cpool1 - cpool0)  

            !! do labile carbon leaching only for kinetics=3
            !! the following leachate transport calculations caused mass imbalance: disabled temporarily
            !   if(kinetics==3) then
            !      cfluxa(:)=0.0      
            !      do ns=1,ms
            !         cfluxa(ns) = sqrt(micinput%wavg(np,ns)/micinput%porosity(np,ns)) * micparam%tvac(np,ns) * miccpool%cpool(np,ns,7) * delty
            !         cfluxa(ns) = 0.0
            !         miccpool%cpool(np,ns,7) = miccpool%cpool(np,ns,7) - cfluxa(ns)                  
            !         if(ns==1) then
            !            miccpool%cpool(np,ns,7) = miccpool%cpool(np,ns,7) - cfluxa(ns)
            !         else
            !            miccpool%cpool(np,ns,7) = miccpool%cpool(np,ns,7) + cfluxa(ns-1) -cfluxa(ns)        
            !         endif               
            !      enddo   
            !     ! converting flux from mg C cm-3 delty-1 to g C m-2 delty-1
            !      micoutput%fluxcleach(np) = micoutput%fluxcleach(np) + cfluxa(ms) * zse(ms) * 1000.0
            !   endif

            ! only do leaching the bottom layer, as other layers are done via bioturb
               cfluxa(:) = 0.0
               ! "tvac" in hourly, so times 24 to convert into daily rate
               cfluxa(ms) = micparam%tvac(np,ms) * sqrt(micinput%wavg(np,ms)/micinput%porosity(np,ms))  &
                          * max(0.0, miccpool%cpool(np,ms,7)) * delty * 24.0
            ! converting flux from mg C cm-3 delty-1 to g C m-2 delty-1
               micoutput%fluxcleach(np) = cfluxa(ms) * zse(ms) * 1000.0
               miccpool%cpool(np,ms,7)  = miccpool%cpool(np,ms,7)  - cfluxa(ms) 

                if(diag==1) then  
                   print *, 'year day site np1', year, i, outp,micparam%diffsocx(outp)
                    do ns=1,ms
                       print *, ns, miccpool%cpool(outp,ns,:) 
                    enddo  
                endif
  
               do ip=1,mcpool
                  do ns=1,ms
                     ypooli(ns) = miccpool%cpool(np,ns,ip)      ! in mg c/cm3
                  enddo  !"ns"
            
                  fluxsoc(:) = 0.0  ! This flux is added in "modelx"
                  diffsocxx= micparam%diffsocx(np)
            
                  call bioturb(int(delty/delty),ms,zse,delty,diffsocxx,fluxsoc,ypooli,ypoole)  ! only do every 24*delt
            
                  do ns=1,ms
                     miccpool%cpool(np,ns,ip) = ypoole(ns)
                  enddo
               enddo ! "ip=1,mcpool"
 
            ! print out the time series of pool sizes
            ! if(micglobal%bgctype(np)==bgcopt) then
            !   write(*,201) year, np, miccpool%cpool(np,1,:),miccpool%cpool(np,ms,:)
201             format('vmicsoil:cpool',2(i5,1x),30(f7.4,1x))               
            ! endif 
         enddo   !"i: day of year (ntime)"
      enddo !"year (nyeqpool)"
    
   enddo !" station_index(station_count)"  
!$OMP END DO
!$OMP END PARALLEL	

     miccpool%cpooleq(:,:,:) = miccpool%cpool(:,:,:)
     
    ! call vmic_output_write(foutput,micinput,micoutput)
    ! call vmic_restart_write(frestart_out,miccpool,micnpool)

    deallocate(xpool0,xpool1)
    deallocate(ypooli,ypoole,fluxsoc,cfluxa)

    end subroutine vmicsoil_hwsd_cpu

   subroutine vmicsoil_hwsd_gpu(jrestart,frestart_in,frestart_out,foutput,kinetics,isoc14,bgcopt,nyeqpool, &
                    zse,micpxdef,micpdef,micparam,micinput,micglobal,miccpool,micnpool,micoutput)
    use mic_constant 
    use mic_variable

    implicit none
    TYPE(mic_param_xscale),  INTENT(INOUT)   :: micpxdef      
    TYPE(mic_param_default), INTENT(IN)      :: micpdef  
    TYPE(mic_parameter),     INTENT(INout)   :: micparam
    TYPE(mic_input),         INTENT(INout)   :: micinput
    TYPE(mic_global_input),  INTENT(INout)   :: micglobal
    TYPE(mic_cpool),         INTENT(INOUT)   :: miccpool
    TYPE(mic_npool),         INTENT(INOUT)   :: micnpool
    TYPE(mic_output),        INTENT(INout)   :: micoutput
    real(r_2) zse(ms)
    integer isoc14,kinetics,bgcopt,nyeqpool
    ! local variables
  !  real(r_2),    dimension(:), allocatable  :: xpool0,xpool1
  !  real(r_2),    dimension(:), allocatable  :: ypooli,ypoole,fluxsoc,cfluxa

    real(r_2),    dimension(mcpool)  :: xpool0,xpool1
    real(r_2),    dimension(ms)      :: ypooli,ypoole,fluxsoc,cfluxa
  
    integer       ndelt,i,j,year,ip,np,ns,ny
    integer       nyrun
    real(r_2)     timex,delty,fluxdocsx,diffsocxx

    integer    jrestart
    character*140 frestart_in,frestart_out,foutput
    real(r_2)  cpool0, cpool1, totcinput  

   ! local variables
   ! for numerical solution
   real(r_2),    parameter            :: tol = 1.0E-04
   real(r_2),    parameter            :: tolx = 0.0001
   real(r_2),    parameter            :: tolf = 0.000001
   integer,      parameter            :: ntrial = 100

   ! local variables
   real(r_2)                      deltD !,tot0,tot1,totflux
   real(r_2), dimension(ms)    :: xzse
   real(r_2), dimension(ms+1)  :: sdepthx
   real(r_2)                      coeffA, coeffB
   real(r_2), dimension(ms)    :: at,bt,ct,rt
   real(r_2), dimension(ms)    :: xpool
   real(r_2)  cleachloss

  !  allocate(xpool0(mcpool),xpool1(mcpool))
  !  allocate(ypooli(ms),ypoole(ms),fluxsoc(ms),cfluxa(ms))
  !  allocate(xzse(ms))
  !  allocate(sdepthx(ms+1))
  !  allocate(at(ms),bt(ms),ct(ms),rt(ms))
  !  allocate(xpool(ms))

      call vmic_param_constant(kinetics,micpxdef,micpdef,micparam,zse) 
      call vmic_init(miccpool,micnpool)
      call vmic_param_time(kinetics,micpxdef,micpdef,micparam,micinput,micnpool)  
      
      if(jrestart==1) call vmic_restart_read(miccpool,micnpool,frestart_in)      
  
      ndelt   = int(24*365/delt) ! number of time step per year in "delt" unit
      
! "data copyin" : all data accessed by GPU
! "create": intermediate variables used by GPU
! "copyout" copy data out from GPU
! "private": every cell-dependent variables used in paralelling computing  

!$acc data copyin(micpdef,micparam,miccpool,micinput,micoutput,micglobal,  &
!$acc bgcopt,ndelt,zse,kinetics,nyeqpool,isoc14)      &
!$acc create(delty,timex,fluxsoc,year,ny,i,ns,np,ip,xpool0,xpool1,ypooli,ypoole,diffsocxx,cfluxa, &
!$acc j,deltD,xzse,sdepthx,coeffA,coeffB,at,bt,ct,rt,xpool,cleachloss)              &
!$acc copyout(miccpool%cpool,micoutput%fluxcinput,micoutput%fluxrsoil,micoutput%fluxcleach)
!$acc PARALLEL LOOP                                                      &
!$acc private(delty,timex,fluxsoc,year,ny,i,ns,ip,np,xpool0,xpool1,ypooli,ypoole,diffsocxx,cfluxa,&
!$acc j,deltD,xzse,sdepthx,coeffA,coeffB,at,bt,ct,rt,xpool,cleachloss)

      do np=1,mp
      
         if(micparam%bgctype(np)==bgcopt .and. micglobal%area(np)>0.0) then   ! optimizing parameters for each soil order

         do year=1,nyeqpool
            ny = year-nyeqpool
            micoutput%fluxcinput(np)=0.0; micoutput%fluxrsoil(np) = 0.0; micoutput%fluxcleach(np)= 0.0    ! yearly fluxes
             
            do i=1,365         

               call variable_time(year,i,micglobal,micinput,micnpool)
      
               ! calculate parameter values that depend on soil temperature or moisture (varying with time)
               call vmic_param_time(kinetics,micpxdef,micpdef,micparam,micinput,micnpool)   

               ! for each soil layer
               ! sum last all C pools of all layers for compute the soil respiration = input - sum(delCpool)
               ! before leaching is computed
                cpool0 =0.0; cpool1 =0.0; totcinput = 0.0            
               do ns=1,ms
                 ! micinput%cinputm(np,ns)+micinput%cinputs(np,ns) in mg C/cm3/delt
                  totcinput =totcinput + (micinput%cinputm(np,ns)+micinput%cinputs(np,ns)) *1000.0 * zse(ns)   ! convert to g C/m2/delt/zse

                  do ip=1,mcpool
                     xpool0(ip) = miccpool%cpool(np,ns,ip)
                     cpool0     = cpool0  + xpool0(ip) * zse(ns) * 1000.0  ! 1000 for mg C/cm3 to g C/m2/zse                     
                  enddo

                 ! here the integration step is "delty" in rk4 and "ndelt" is number of "delt (hour) per year 
                  timex=real(i*delt)
                  delty = real(ndelt)/(365.0*delt)  ! time step in rk4 in "24 * delt (or daily)", all C input are in " per delt"
                 
                  call rk4modelx(timex,delty,ny,isoc14,np,ns,kinetics,micpdef,micparam,micinput,xpool0,xpool1)  
                 
                  do ip=1,mcpool
                     miccpool%cpool(np,ns,ip) = max(xpool1(ip),1.0e-8)
                     cpool1 = cpool1 + miccpool%cpool(np,ns,ip) * zse(ns) * 1000.0  ! 1000 for mg C/cm3 to g C/m2/zse                        
                  enddo        

               enddo    ! "ns"

               micoutput%fluxcinput(np)= micoutput%fluxcinput(np) + totcinput * real(delty)        
               micoutput%fluxrsoil(np) = micoutput%fluxrsoil(np)  + totcinput * real(delty) + (cpool1 - cpool0)  

               ! do labile carbon leaching only for kinetics=3
               ! the following leachate transport calculations caused mass imbalance: disabled temporarily
            !   if(kinetics==3) then
            !     cfluxa(:)=0.0      
            !     do ns=1,ms
            !        cfluxa(ns) = sqrt(micinput%wavg(np,ns)/micinput%porosity(np,ns)) * micparam%tvac(np,ns) * miccpool%cpool(np,ns,7) * delty               
            !        if(ns==1) then
            !           miccpool%cpool(np,ns,7) = miccpool%cpool(np,ns,7) - cfluxa(ns)
            !        else
            !           miccpool%cpool(np,ns,7) = miccpool%cpool(np,ns,7) + cfluxa(ns-1) -cfluxa(ns)        
            !        endif               
            !     enddo   
            !    ! converting flux from mg C cm-3 delty-1 to g C m-2 delty-1
            !    micoutput%fluxcleach(np) = micoutput%fluxcleach(np) + cfluxa(ms) * zse(ms) * 1000.0
            !  endif

                if(diag==1) then  
                   print *, 'year day site np1', year, i, outp,micparam%diffsocx(outp)
                    do ns=1,ms
                       print *, ns, miccpool%cpool(outp,ns,:) 
                    enddo  
                endif
  
               do ip=1,mcpool
                  do ns=1,ms
                     ypooli(ns) = miccpool%cpool(np,ns,ip)      ! in mg c/cm3
                  enddo  !"ns"
            
                  fluxsoc(:) = 0.0  ! This flux is added in "modelx"
                  diffsocxx= micparam%diffsocx(np)
            
                 !Move bioturb here to work around memory auto allocation failure
                 !call bioturb(int(delty/delty),ms,zse,delty,diffsocxx,fluxsoc,ypooli,ypoole)  ! only do every 24*delt
                 !subroutine bioturb(ndelt,ms,zse,delt,diffsocxx,fluxsoc,xpooli,xpoole)
                  
                  sdepthx(1) = 0.0          ! depth of a layer from the top (x_0.5=0.0 eg soil surface)
                  do j=2,ms+1
                     sdepthx(j) = sdepthx(j-1) + zse(j-1)*100.0     ! depth of the bottom of each layer (eg x_j+0.5)
                                                                    !*100 to convert from m to cm
                  enddo
             
                  do j=1,ms
                     xzse(j) = 0.5 * (sdepthx(j) + sdepthx(j+1))    ! depth of midpoint of a layer j  (x_j)
                  enddo
             
                  deltD = diffsocxx * delty
                  xpool = ypooli
               
                  !do i=1,1 ( int(delty/delty) == 1 )
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
                  !enddo
                  ypoole = xpool
                  
                  !!! end bioturb
            
                  do ns=1,ms
                     miccpool%cpool(np,ns,ip) = ypoole(ns)
                  enddo
               enddo ! "ip=1,mcpool"
   
               ! computing daily leaching loss from bottom-layer LWMC 
               cleachloss = micparam%tvac(np,ms) * sqrt(micinput%wavg(np,ms)/micinput%porosity(np,ms)) *  miccpool%cpool(np,ms,7)  * 24.0
               cleachloss = max(0.0,min(cleachloss,miccpool%cpool(np,ms,7)))
               micoutput%fluxcleach(np) = micoutput%fluxcleach(np) + cleachloss
               miccpool%cpool(np,ms,7)  = miccpool%cpool(np,ms,7)  - cleachloss               
               
            enddo   ! "day"      
         enddo !"year"
      endif   !bgctype(np)=bgcopt
   enddo !"mp"                                                                                                       
!$ACC END PARALLEL
!$ACC END DATA 
  
    miccpool%cpooleq(:,:,:) = miccpool%cpool(:,:,:)
     
   !  call vmic_output_write(foutput,micinput,micoutput)
   !  call vmic_restart_write(frestart_out,miccpool,micnpool)
  !  deallocate(xpool0,xpool1)
  !  deallocate(ypooli,ypoole,fluxsoc,cfluxa)
  !  deallocate(xzse)
  !  deallocate(sdepthx)
  !  deallocate(at,bt,ct,rt)
  !  deallocate(xpool)
    
    end subroutine vmicsoil_hwsd_gpu

! ##############mesc_interface.f90###########################