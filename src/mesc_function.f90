!> functions to be called by the function functn called from main.for
!! functn: called from main.for with the following options from "case.txt"
!! functn_c14 (case=1):       12C and 14C                   
!! functn_frc1 (case=2):      SOC fractions                        
!! functn_soc_wosis(case=3):  WOSIS SOC profiles  (not activated)    
!! functn_soc_hwsd (case=4):  hwsd SOC profiles      
!! functn_global (case=5):    global SOC profile (not activated) 
!! ###############mesc_function.f90###########################
!
 real*8 function functn_c14(nx,xparam16)
   use mic_constant
   use mic_variable
   implicit none
    TYPE(mic_param_xscale)    :: micpxdef
    TYPE(mic_param_default)   :: micpdef
    TYPE(mic_parameter)       :: micparam
    TYPE(mic_input)           :: micinput
    TYPE(mic_global_input)    :: micglobal
    TYPE(mic_cpool)           :: miccpool
    TYPE(mic_npool)           :: micnpool
    TYPE(mic_output)          :: micoutput

    !local variables
    real*8,    dimension(16)           :: xparam16
    integer    nx
    integer,   dimension(16)           :: nxopt
    real*8,    dimension(16)           :: xopt
    real*8     totcost1,totcost2
    integer    ifsoc14,kinetics,bgcopt,jopt,nyeqpool,isoc14,jglobal,jmodel
    integer jrestart,nparam
    character*140 frestart_in,frestart_out,foutput
    character*140 frac14c,f14c(5),filecluster
    real(r_2), dimension(:), allocatable :: zse

      jrestart=0;xopt(:)=1.0
      do nparam=1,16
         nxopt(nparam) = nparam
      enddo
      
      frestart_in='miccpool_in.nc'
      frestart_out='miccpool_out.nc'
      foutput='vmic_output.nc'

      open(1,file='params1.txt')
!      open(91,file='modobs.txt')
!      open(92,file='modobs2.txt')
!      open(93,file='modobs_c14.txt')
!      open(94,file='modobs2_c14.txt')
      read(1,*) 
      read(1,*) jglobal,ifsoc14,kinetics,bgcopt,jopt,jrestart
      read(1,11) frac14c
      read(1,11) f14c(1)
      read(1,11) f14c(2)
      read(1,11) f14c(3)
      read(1,11) f14c(4)
      read(1,11) f14c(5)
      read(1,11) filecluster
11    format(a140)
      read(1,*) xopt(1:14)
      
      if(jopt==0) then
         read(1,*) nxopt(1:nx)
         do nparam=1,nx
            xopt(nxopt(nparam)) = xparam16(nparam)
         enddo
      endif
    !  print*, xopt

      mp = 213

      
      totcost1 = 0.0; totcost2=0.0
      nyeqpool= 500;jmodel=1;mpft=17;mbgc=12;ntime=1;nlon=1;nlat=1
      ms=15
      allocate(zse(ms))
      zse(1:ms)=0.1
      
      call mic_allocate_parameter(mpft,mbgc,mp,ms,micpxdef,micparam)
      call mic_allocate_input(mp,ms,nlon,nlat,ntime,micinput,micglobal)
      call mic_allocate_output(mp,micoutput)
      call mic_allocate_cpool(mp,ms,miccpool)
      call mic_allocate_npool(mp,ms,micnpool)

          isoc14 = 0
      !    print *, "isoc14 =",isoc14,'--getdata_c14'  
          call getdata_c14(frac14c,f14c,filecluster,micinput,micparam,micnpool,zse)
          call vmic_param_xscale(xopt,bgcopt,jmodel,micpxdef)    
      !    print *, 'vmicsoil_c14'
          call vmicsoil_c14(jrestart,frestart_in,frestart_out,foutput,kinetics,isoc14,ifsoc14,bgcopt,nyeqpool, &
                        zse,micpxdef,micpdef,micparam,micinput,micglobal,miccpool,micnpool,micoutput)

       !   print *, 'calcost_c14'
          call calcost_c14(nx,isoc14,bgcopt,xopt,micparam,miccpool,micinput,zse,totcost1)

          miccpool%c12pooleqp(:) = miccpool%cpooleqp(:)
          miccpool%c12pooleqm(:) = miccpool%cpooleqm(:)

          isoc14 = 1
       !   print *, "isoc14 =",isoc14,'--getdata_c14'  
          call getdata_c14(frac14c,f14c,micinput,micparam,micnpool,zse)
          call vmic_param_xscale(xopt,bgcopt,jmodel,micpxdef)    
       !   print *, 'vmicsoil_c14'
          call vmicsoil_c14(jrestart,frestart_in,frestart_out,foutput,kinetics,isoc14,ifsoc14,bgcopt,nyeqpool+2000, &
                        zse,micpxdef,micpdef,micparam,micinput,micglobal,miccpool,micnpool,micoutput)

        !  print *, 'calcost_c14'
          call calcost_c14(nx,isoc14,bgcopt,xopt,micparam,miccpool,micinput,zse,totcost2)
          functn_c14 = totcost1+totcost2
        !  print *,"tot1 = ",totcost1
        !  print *,"tot2 = ",totcost2
           call screenout('c14run',jmodel,bgcopt,xopt,functn_c14)
      close(1)
!      close(91)
!      close(92)
!      close(93)
!      close(94)
  
!      functn = totcost
      
      call mic_deallocate_parameter(mpft,mbgc,mp,ms,micpxdef,micparam)
      call mic_deallocate_input(mp,ms,nlon,nlat,ntime,micinput,micglobal)
      call mic_deallocate_output(mp,micoutput)
      call mic_deallocate_cpool(mp,ms,miccpool)
      call mic_deallocate_npool(mp,ms,micnpool) 
      deallocate(zse)      
END function functn_c14


real*8 function functn_frc1(nx,xparam16)
   ! only one layer without bioturbation for SOC fraction data only
   use mic_constant
   use mic_variable
   implicit none
    TYPE(mic_param_xscale)    :: micpxdef
    TYPE(mic_param_default)   :: micpdef
    TYPE(mic_parameter)       :: micparam
    TYPE(mic_input)           :: micinput
    TYPE(mic_global_input)    :: micglobal
    TYPE(mic_cpool)           :: miccpool
    TYPE(mic_npool)           :: micnpool
    TYPE(mic_output)          :: micoutput

    !local variables
    integer    nx
    integer,   dimension(16)           :: nxopt
    real*8,    dimension(16)           :: xparam16
    real*8,    dimension(16)           :: xopt
    real*8     totcost1
    integer    ifsoc14,kinetics,bgcopt,jopt,nyeqpool,isoc14,jglobal,jmodel
    integer jrestart,nparam
    character*140 frestart_in,frestart_out,foutput
    character*140 cfraction,filecluster
    real(r_2), dimension(:), allocatable :: zse
    integer mpx
    
      jrestart=0;xopt(:)=1.0
      do nparam=1,16
         nxopt(nparam) = nparam
      enddo
      
      frestart_in='miccpool_in.nc'
      frestart_out='miccpool_out.nc'
      foutput='vmic_output.nc'

      open(1,file='params1.txt')
      read(1,*) 
      read(1,*) jglobal,ifsoc14,kinetics,bgcopt,jopt,jrestart
      read(1,11) cfraction
      read(1,11) filecluster
11    format(a140)
      read(1,*) xopt(1:14)
      
      if(jopt==0) then
         read(1,*) nxopt(1:nx)
         do nparam=1,nx
            xopt(nxopt(nparam)) = xparam16(nparam)
         enddo
      endif
    !  print *, xopt
      
      close(1)
      !mp = 2206
      ntime=1
      
      totcost1 = 0.0
      nyeqpool= 1000
      isoc14 = 0
      jmodel=1;mpft=17;mbgc=12;nlon=1;nlat=1
      ms = 10      
      allocate(zse(ms))
      zse(1) =0.02;zse(2)=0.04;zse(3)=0.06;zse(4)=0.08
      zse(5:8)=0.2;zse(9:10)=0.5
      call getdata_frc_dim(cfraction,mpx)
      mp = mpx
      call mic_allocate_parameter(mpft,mbgc,mp,ms,micpxdef,micparam)
      call mic_allocate_input(mp,ms,nlon,nlat,ntime,micinput,micglobal)
      call mic_allocate_output(mp,micoutput)
      call mic_allocate_cpool(mp,ms,miccpool)
      call mic_allocate_npool(mp,ms,micnpool)

          
    !  print *, "isoc14 =",isoc14,'--getdata_frc'  
      call getdata_frc(cfraction,filecluster,jglobal,bgcopt,micinput,micparam,micnpool,zse)
      call vmic_param_xscale(xopt,bgcopt,jmodel,micpxdef)    

    !  print *, 'vmicsoil_frc1_cpu'
      call vmicsoil_frc1_cpu(jrestart,frestart_in,frestart_out,foutput,kinetics,isoc14,ifsoc14,bgcopt,nyeqpool, &
                        zse,micpxdef,micpdef,micparam,micinput,micglobal,miccpool,micnpool,micoutput)

    !  print *, 'calcost_frc1'
      call calcost_frc1(nx,bgcopt,xopt,micpxdef,micparam,miccpool,micinput,micglobal,zse,totcost1)
      
      close(1)

      functn_frc1    = totcost1
      call screenout('fraction',jmodel,bgcopt,xopt,functn_frc1)
      call mic_deallocate_parameter(mpft,mbgc,mp,ms,micpxdef,micparam)
      call mic_deallocate_input(mp,ms,nlon,nlat,ntime,micinput,micglobal)
      call mic_deallocate_output(mp,micoutput)
      call mic_deallocate_cpool(mp,ms,miccpool)
      call mic_deallocate_npool(mp,ms,micnpool) 
      deallocate(zse)        
END function functn_frc1


  real*8 function functn_soc_hwsd(nx,xparam16)
 ! this function is yet to bet set up for running with SCE_UA optimization
   use mic_constant
   use mic_variable
   implicit none
    !local variables
    integer    nx
    integer,   dimension(16)           :: nxopt
    real*8,    dimension(16)           :: xparam16
    real*8,    dimension(16)           :: xopt   
    TYPE(mic_param_xscale)    :: micpxdef
    TYPE(mic_param_default)   :: micpdef
    TYPE(mic_parameter)       :: micparam
    TYPE(mic_input)           :: micinput
    TYPE(mic_global_input)    :: micglobal  
    TYPE(mic_cpool)           :: miccpool
    TYPE(mic_npool)           :: micnpool
    TYPE(mic_output)          :: micoutput

    !local variables
    integer    ifsoc14,kinetics,bgcopt,jopt,nyeqpool,isoc14,jglobal,jmodel
    integer jrestart,nf,ok,nparam,mpx,timex
    character*140  frestart_in,frestart_out,fparam_global,foutput
    character*140 fhwsdsoc,filecluster,fmodis
    real(r_2)     totcost1
    integer       ns
    real(r_2), dimension(:), allocatable :: zse

    
      isoc14=0;nyeqpool = 500;ok=0;totcost1=0.0

      jrestart=0;xopt(:)=1.0
      do nparam=1,16
         nxopt(nparam) = nparam
      enddo

      frestart_in='miccpool_in.nc'
      frestart_out='miccpool_out.nc'
      foutput='vmic_output.nc'
      
    !  open(91,file='modobs.txt')
    !  open(92,file='modobs2.txt')

      open(1,file='params1.txt')      
      read(1,*) 
      read(1,*) jglobal,ifsoc14,kinetics,bgcopt,jopt,jrestart,jmodel
      read(1,101) fhwsdsoc
      read(1,101) filecluster      
      read(1,101) fmodis
      read(1,*)   xopt(1:14)
      read(1,*) nxopt(1:nx)
      do nparam=1,nx
         xopt(nxopt(nparam)) = xparam16(nparam)
      enddo
      close(1)      

!      print *, 'nx xparam16 =', nx, nxopt(1:nx),xparam16(1:nx)
!      print *, 'parameter values used= ', xopt
!      print *, 'ms zse', ms, zse(:)
101   format(a140)      

      ! get dimensions
      call getdata_hwsd_dim(fhwsdsoc,mpx,timex)
      mp=mpx
      ntime=timex
      if(jmodel==1) mpft=17   !CABLE
      if(jmodel==2) mpft=18   !ORCHIDEE
      mbgc=12;nlon=1;nlat=1
      ms=10
      allocate(zse(ms))
      zse(1)=0.02;zse(2)=0.04;zse(3)=0.06;zse(4)=0.08
      zse(5:8)=0.2;zse(9:10)=0.5
      
      
      call mic_allocate_parameter(mpft,mbgc,mp,ms,micpxdef,micparam)
      call mic_allocate_input(mp,ms,nlon,nlat,ntime,micinput,micglobal)
      call mic_allocate_output(mp,micoutput)
      call mic_allocate_cpool(mp,ms,miccpool)
      call mic_allocate_npool(mp,ms,micnpool)

      call getdata_hwsd(fhwsdsoc,filecluster,fmodis,jglobal,bgcopt,jopt,jmodel,micparam,micglobal,zse)
      
      !  call profile()
      call vmic_param_xscale(xopt,bgcopt,jmodel,micpxdef) 

      call vmicsoil_hwsd_cpu(jrestart,frestart_in,frestart_out,foutput,kinetics,isoc14,bgcopt,nyeqpool, &
                         zse,micpxdef,micpdef,micparam,micinput,micglobal,miccpool,micnpool,micoutput)

      call calcost_hwsd2(nx,bgcopt,xopt,micpxdef,micparam,miccpool,micinput,micglobal,zse,totcost1)      
      call mic_deallocate_parameter(mpft,mbgc,mp,ms,micpxdef,micparam)
      call mic_deallocate_input(mp,ms,nlon,nlat,ntime,micinput,micglobal)
      call mic_deallocate_output(mp,micoutput)
      call mic_deallocate_cpool(mp,ms,miccpool)
      call mic_deallocate_npool(mp,ms,micnpool) 

      call screenout('hwsd_soc',jmodel,bgcopt,xopt,totcost1)      
        

    !  close(91)
    !  close(92)
      functn_soc_hwsd = totcost1

      deallocate(zse)
END function functn_soc_hwsd


real*8 function functn(nx,xparam16)
   use mic_constant
   use mic_variable
   implicit none
   !local variables
    integer    nx,runcase
    real*8,    dimension(16)  :: xparam16
    interface
      real*8 function functn_c14(nx, xparam16)
      integer nx
      real*8, dimension(16) :: xparam16
      end function functn_c14

      real*8 function functn_frc1(nx, xparam16)
      integer nx
      real*8, dimension(16) :: xparam16
      end function functn_frc1

      real*8 function functn_soc_wosis(nx, xparam16)
      integer nx
      real*8, dimension(16) :: xparam16
      end function functn_soc_wosis

      real*8 function functn_soc_hwsd(nx, xparam16)
      integer nx
      real*8, dimension(16) :: xparam16
      end function functn_soc_hwsd
      
      real*8 function functn_global(nx, xparam16)
      integer nx
      real*8, dimension(16) :: xparam16
      end function functn_global
    end interface
    
    open(1,file='case.txt')
    read(1,*) runcase
    close(1)
    SELECT CASE (runcase)
      CASE (1)    ! run model for 14C                                
        functn = functn_c14(nx,xparam16)
      CASE (2)    ! run model for POC/MAOC fractions
        functn = functn_frc1(nx,xparam16)  
    !  CASE(3)     ! run model for WOSIS SOC profile 
    !    functn = functn_soc_wosis(nx,xparam16)    
      CASE (4)    ! run model for HWSD SOC profile with CABLE/ORCHIDEE input 
        functn = functn_soc_hwsd(nx,xparam16)  
     ! CASE (5)  ! run model for CABLE/ORCHIDEE cells 
     !   functn = functn_global(nx,xparam16)  
    END SELECT  
    
 END function functn


! ###############mesc_function.f90###########################
