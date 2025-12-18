! ##############mesc_cost.f90###########################
    subroutine calcost_c14(nx,isoc14,bgcopt,xopt,micparam,miccpool,micinput,zse,totcost)
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_parameter), INTENT(INOUT) :: micparam
    TYPE(mic_cpool),     INTENT(INOUT) :: miccpool
    TYPE(mic_input),     INTENT(IN)    :: micinput
    real(r_2) zse(ms)
    integer nx,isoc14,bgcopt
    real*8  totcost
    real*8, dimension(16)              :: xopt

    ! cost function
    real(r_2), dimension(:), allocatable           :: xcost,xobs,xobsp,xobsm 
    real(r_2), dimension(:), allocatable           :: xmod,xmodp,xmodm !! weighted modelled SOC, POC and MAOC
    real(r_2), dimension(:), allocatable           :: xfracpmod,xfracmmod,xfracpobs,xfracmobs
    integer   np,ns,ip
    real(r_2)  xbdz,small

    real(r_2) xtop,xbot,x1,x2 !! cm
    real(r_2) weight
 


      allocate(xcost(mp),xobs(mp),xobsp(mp),xobsm(mp))
      allocate(xmod(mp),xmodp(mp),xmodm(mp))
      allocate(xfracpmod(mp),xfracmmod(mp),xfracpobs(mp),xfracmobs(mp))
      
      small = 1.0e-6
      xcost(:)=0.0 
      xobs(:)=0.0; xobsp(:)=0.0;xobsm(:)=0.0

      do np=1,mp
         if(micparam%bgctype(np)==bgcopt) then
            xobs(np)  = micparam%csoilobs(np,1)   !! only one observation for each site
            xobsp(np) = micparam%csoilobsp(np,1)
            xobsm(np) = micparam%csoilobsm(np,1)
            xfracpobs(np) = min(1.0,max(0.0,xobsp(np)/(xobsp(np)+xobsm(np))))
            xfracmobs(np) = 1.0 - xfracpobs(np)

           !! calculated weighted average of modelled values to correspond to observations
            xmod(np)=0.0; xmodp(np)=0.0; xmodm(np)=0.0
            xtop = real(micparam%top(np))*0.01   ! convert from cm to m
            xbot = real(micparam%bot(np))*0.01  

            x1 =0.0; x2 =0.0
            do ns=1,ms
               x2=x1+zse(ns)
               if (x1 <= xbot .and. x2 >= xtop) then
                  weight = min(xbot,x2) - max(xtop,x1)
                  xmod(np)  = xmod(np)  + weight * sum(miccpool%cpooleq(np,ns,3:9)) !! unit: mg C cm-3
                  xmodp(np) = xmodp(np) + weight * (sum(miccpool%cpooleq(np,ns,3:5))+sum(miccpool%cpooleq(np,ns,7:8))) !! observed poc = modelled (aggregate c + lwmc)
                  xmodm(np) = xmodm(np) + weight * (miccpool%cpooleq(np,ns,6) + miccpool%cpooleq(np,ns,9))             !! observed maoc = modelled maoc
               end if
               x1=x2
            enddo
           ! this assumes bulk density constant in the vertical
           xmod(np)   = 1000.0 * xmod(np) /((xbot-xtop) * micinput%bulkd(np,1))   !! unit: g C/kg soil
           xmodp(np)  = 1000.0 * xmodp(np)/((xbot-xtop) * micinput%bulkd(np,1))
           xmodm(np)  = 1000.0 * xmodm(np)/((xbot-xtop) * micinput%bulkd(np,1))
           
           xfracpmod(np) = min(1.0,max(0.0,xmodp(np)/(xmodp(np)+xmodm(np))))
           xfracmmod(np) = 1.0 - xfracpmod(np)
           
           miccpool%cpooleqp(np) = xmodp(np)
           miccpool%cpooleqm(np) = xmodm(np)
           ! avoid dividing by zero
           miccpool%c12pooleqp(np) = max(small,miccpool%c12pooleqp(np))
           micparam%c14soilobsp(np)= max(small,micparam%c14soilobsp(np))
           miccpool%c12pooleqm(np) = max(small,miccpool%c12pooleqm(np))
           micparam%c14soilobsm(np)= max(small,micparam%c14soilobsm(np))

           if (xmod(np) >= 1000.0 .or. xmodp(np) >= 1000.0 .or. xmodm(np) >= 1000.0) then
               print *, 'abnormal value of model simulation site=', micparam%siteid(np)
               print *, 'parameter values = ',  xopt(1:nx)
            !   stop
           endif
            
           if(isoc14 == 0) then
             if((xobsp(np)+xobsm(np))>0.0 .and. (xobsp(np)+xobsm(np))<1000.0) then
        !     xcost(np) = xcost(np) + ((xmodp(np) - xobsp(np))/xobsp(np))**2 +((xmodm(np) - xobsm(np))/xobsm(np))**2
              xcost(np) = xcost(np) + (xfracpmod(np)-xfracpobs(np))** 2 + (xfracmmod(np) - xfracmobs(np))**2
             endif
             write(91,901) micparam%siteid(np),micparam%pft(np),micparam%top(np),micparam%bot(np),xobs(np),xmod(np),xobsp(np),xmodp(np),xobsm(np),xmodm(np)
                  
             do ns = 1,ms
                write(92,*) micparam%siteid(np),micparam%pft(np), ns, (1000.0*miccpool%cpooleq(np,ns,ip)/micinput%bulkd(np,ns),ip=1,mcpool)
             enddo

           else 
             xcost(np) = xcost(np) +(micparam%c14soilobsp(np) - xmodp(np)/miccpool%c12pooleqp(np))**2  &
                                   +(micparam%c14soilobsm(np) - xmodm(np)/miccpool%c12pooleqm(np))**2
                 
             write(93,901) micparam%siteid(np),micparam%pft(np),micparam%top(np),micparam%bot(np), &
                           xfracpobs(np),xfracpmod(np),xfracmobs(np),xfracmmod(np),                &
                           micparam%c14soilobsp(np),xmodp(np)/miccpool%c12pooleqp(np),             &
                           micparam%c14soilobsm(np),xmodm(np)/miccpool%c12pooleqm(np)

             do ns = 1,ms
                write(94,*) micparam%siteid(np),micparam%pft(np), ns, (1000.0*miccpool%cpooleq(np,ns,ip)/micinput%bulkd(np,ns),ip=1,mcpool)
             enddo
           endif 
                        
        endif  !bgctype(np)=bgcopt
      enddo  !"np"
      totcost = sum(xcost(1:mp))

      deallocate(xcost,xobs,xobsp,xobsm)
      deallocate(xmod,xmodp,xmodm)
      deallocate(xfracpmod,xfracmmod,xfracpobs,xfracmobs)

901   format(i5,2x,3(i4,2x),12(f10.4,2x))
921   format(i7,2x,2(i4,2x),10(f12.4,1x))
    end subroutine calcost_c14


    SUBROUTINE calcost_frc1(nx,bgcopt,xopt,micpxdef,micparam,miccpool,micinput,zse,totcost)
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_param_xscale),INTENT(IN)  :: micpxdef
    TYPE(mic_parameter), INTENT(IN)    :: micparam
    TYPE(mic_cpool),     INTENT(INOUT) :: miccpool
    TYPE(mic_input),     INTENT(IN)    :: micinput
    real(r_2) zse(ms)
    integer nx,bgcopt
    real*8  totcost
    real*8, dimension(16)              :: xopt

    ! cost function
    real(r_2), dimension(:), allocatable        :: xcost,xobs,xobsp,xobsm 
    real(r_2), dimension(:), allocatable        :: xmod,xmodp,xmodm !! weighted modelled SOC, POC and MAOC
    integer   np,ns,ip,ipsite
    real(r_2)  xbdz

    real(r_2) xtop,xbot,x1,x2 !! cm
    real(r_2) weight
    real(r_2),dimension(:), allocatable         :: xmodfracp,xmodfracm,xobsfracp,xobsfracm
 
 
      allocate(xcost(mp),xobs(mp),xobsp(mp),xobsm(mp))
      allocate(xmod(mp),xmodp(mp),xmodm(mp))
      allocate(xmodfracp(mp),xmodfracm(mp),xobsfracp(mp),xobsfracm(mp))
      
      
      ipsite=-999
      xcost(:)=0.0 
      xobs(:)=0.0; xobsp(:)=0.0;xobsm(:)=0.0;xmodfracp(:)=0.0;xmodfracm(:)=0.0;xobsfracp(:)=0.0;xobsfracm(:)=0.0

      do np=1,mp
         if(micparam%bgctype(np)==bgcopt) then
            ipsite=np         
            xobs(np)  = micparam%csoilobs(np,1) !! only one observation for each site
            xobsp(np) = micparam%csoilobsp(np,1)
            xobsm(np) = micparam%csoilobsm(np,1)

           !! calculated weighted average of modelled values to correspond to observations
            xmod(np)=0.0; xmodp(np)=0.0; xmodm(np)=0.0
            xtop = real(micparam%top(np))*0.01   ! convert from cm to m
            xbot = real(micparam%bot(np))*0.01  

            x1 =0.0; x2 =0.0
            do ns=1,ms
               x2=x1+zse(ns)
               if (x1 <= xbot .and. x2 >= xtop) then
                  weight = min(xbot,x2) - max(xtop,x1)
                  xmod(np)  = xmod(np)  + weight * sum(miccpool%cpooleq(np,ns,1:9)) !! unit: mg C cm-3
                  xmodp(np) = xmodp(np) + weight * (sum(miccpool%cpooleq(np,ns,1:5))+sum(miccpool%cpooleq(np,ns,7:8))) !! observed poc = modelled (aggregate c + lwmc)
                  xmodm(np) = xmodm(np) + weight * (miccpool%cpooleq(np,ns,6) + miccpool%cpooleq(np,ns,9))             !! observed maoc = modelled maoc
               end if
               x1=x2
            enddo

           xmod(np)   = 1000.0 * xmod(np) /((xbot-xtop)*micinput%bulkd(np,1))   !! unit: g C/kg soil
           xmodp(np)  = 1000.0 * xmodp(np)/((xbot-xtop)*micinput%bulkd(np,1))
           xmodm(np)  = 1000.0 * xmodm(np)/((xbot-xtop)*micinput%bulkd(np,1))
            
           xmodfracp(np) = xmodp(np)/(xmodp(np)+xmodm(np)+1.0e-6)
           xmodfracm(np) = xmodm(np)/(xmodp(np)+xmodm(np)+1.0e-6)
           xobsfracp(np) = xobsp(np)/(xobsp(np)+xobsm(np)+1.0e-6)
           xobsfracm(np) = xobsm(np)/(xobsp(np)+xobsm(np)+1.0e-6)

           if (xmod(np) >= 1000.0 .or. xmodp(np) >= 1000.0 .or. xmodm(np) >= 1000.0) then
               print *, 'abnormal value of model simulation site=', micparam%siteid(np)
               print *, 'parameter values = ',  xopt(1:nx)
               print *, xmodp(np),xmodm(np)
            !   stop
            endif
            

            if(xobsp(np) >0.0 .and. xobsm(np)>0.0 .and. xobsfracm(np) >1.0e-10) then  !! POC is not available for some sites
            !   xcost(np) = xcost(np) + 400.0*(xmodfracm(np) - xobsfracm(np))**2 &
            !                         + 0.01 *(xmodm(np)+xmodp(np) - xobsm(np)-xobsp(np))**2
            
            !    xcost(np) = xcost(np) + 2.0 *(log(xmodfracm(np)) - log(xobsfracm(np)))**2 &
            !                          + 0.1 *(log(xmodm(np)+xmodp(np)) - log(xobsm(np)+xobsp(np)))**2
                xcost(np) = xcost(np) + 20.0 *(xmodfracm(np) - xobsfracm(np))**2 &
                                      + 0.1 *(log(xmodm(np)+xmodp(np)) - log(xobsm(np)+xobsp(np)))**2
            endif

            write(91,901) micparam%dataid(np),micparam%siteid(np),micparam%bgctype(np),micparam%top(np),micparam%bot(np), &
                          xobsp(np),xmodp(np),xobsm(np),xmodm(np),xobsfracp(np),xmodfracp(np),xobsfracm(np),xmodfracm(np)
                  
            do ns = 1,ms
               write(92,921) micparam%dataid(np),micparam%siteid(np),micparam%bgctype(np),ns,xtop,xbot,&
                           (1000.0*miccpool%cpooleq(np,ns,ip)/micinput%bulkd(np,ns),ip=1,mcpool)
            enddo

                        
        endif  !bgctype(np)=bgcopt
      enddo  !"np"
      totcost = sum(xcost(1:mp))

      deallocate(xcost,xobs,xobsp,xobsm)
      deallocate(xmod,xmodp,xmodm)
      deallocate(xmodfracp,xmodfracm,xobsfracp,xobsfracm)

901   format(i4,1x,i10,1x,3(i4,1x),10(f12.4,1x))
921   format(i4,1x,i10,1x,2(i3,1x),2(f7.2,1x),14(f10.4,1x))
911   format(2(i7,1x),4(f12.4,1x),4(f9.5,1x),e15.6)

    end SUBROUTINE calcost_frc1

    subroutine calcost_hwsd2(nx,bgcopt,xopt,micpxdef,micparam,miccpool,micinput,micglobal,zse,totcost)
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_param_xscale), INTENT(IN)    :: micpxdef
    TYPE(mic_parameter),    INTENT(IN)    :: micparam
    TYPE(mic_cpool),        INTENT(INOUT) :: miccpool
    TYPE(mic_input),        INTENT(IN)    :: micinput
    TYPE(mic_global_input), INTENT(IN)    :: micglobal    
    real(r_2) zse(ms)    
    integer nx,bgcopt,msobs
    real*8  totcost
    real*8, dimension(16)              :: xopt
    ! cost function
    real(r_2), dimension(:),   allocatable        :: xcost,xtop,xbot
    real(r_2), dimension(:,:), allocatable        :: xmod
    real(r_2), dimension(:,:), allocatable        :: xobs7, xmod7
    real(r_2)                                     :: fracpocm,fracmaocm,fracmicm,fraclabm
    integer   np,ns,ipsite,v,ip
    real(r_2)  xbdz

    msobs=7
    allocate(xcost(mp))
    allocate(xmod(mp,ms))
    allocate(xobs7(mp,msobs), xmod7(mp,msobs),xtop(msobs),xbot(msobs))
    
    ! specific to HWSD_SOC, total 7 layers with thickness of 0.2 for the top 5 layers, and 0.5m for the next 2 layers
    do ns=1,5
       xtop(ns) =(ns-1) * 0.2
       xbot(ns) = ns    * 0.2
    enddo
    xbot(6)=1.5;     xbot(7)=2.0    
    xtop(6)=xbot(5); xtop(7)=xbot(6)    

    xobs7(:,:)=0.0; xmod7(:,:)=0.0;xmod(:,:)=0.0;xcost(:)=0.0
    ipsite=-999
    do np=1,mp
       if(micparam%bgctype(np)==bgcopt .and. micglobal%area(np) > 0.0) then      
         ipsite=np 
         xcost(np) = 0.0
         v = 0

         do ns = 1,ms
            xmod(np,ns) = 1000.0 * sum(miccpool%cpooleq(np,ns,3:mcpool))/micinput%bulkd(np,ns)   ! gC/kg soil
         enddo   

         do ns = 1,msobs
            if(ns==1) then
              xobs7(np,ns) = (micparam%csoilobs(np,2) * zse(2)+ micparam%csoilobs(np,3) * zse(3) + micparam%csoilobs(np,4) * zse(4)) &
                            /(zse(2)+zse(3)+zse(4)) 
              xmod7(np,ns) = (xmod(np,2) * zse(2) +xmod(np,3) * zse(3) + xmod(np,4) * zse(4)) &
                             /(zse(2)+zse(3)+zse(4)) 
            else
              xobs7(np,ns) = micparam%csoilobs(np,ns+3)   
              xmod7(np,ns) = xmod(np,ns+3)                                     ! gC/kg soil
            endif
            
            if(xmod7(np,ns) <0.0 .or. xmod7(np,ns) >1.0e3) then
               print *, 'abnormal value of model simulation site=', micparam%siteid(np)
               print *, 'parameter values = ',  xopt(1:nx)
               print *,  xobs7(np,ns),xmod7(np,ns),xobs7(np,ns)-xmod7(np,ns)
               print *, ' modelled pool size= ', ns,miccpool%cpooleq(np,ns,:)/micinput%bulkd(np,ns)
               !stop
            endif   

            if(xobs7(np,ns) > 0.0 .and. xobs7(np,ns) <1.0e3) then
               xcost(np) = xcost(np) + (log(xobs7(np,ns))-log(xmod7(np,ns)))**2 
            !   xcost(np) = xcost(np) + (xobs7(np,ns)-xmod7(np,ns))**2 

               write(91,901) micparam%siteid(np),micparam%pft(np),micparam%isoil(np),micparam%sorder(np),ns, &
                             xobs7(np,ns),xmod7(np,ns)
            endif
         enddo !"ns"

         do ns = 1,ms
            fracpocm  = (sum(miccpool%cpooleq(np,ns,3:8))-miccpool%cpooleq(np,ns,6))/(sum(miccpool%cpooleq(np,ns,3:mcpool))+1.0e-6)
            fracmaocm = (miccpool%cpooleq(np,ns,6)+miccpool%cpooleq(np,ns,9))/(sum(miccpool%cpooleq(np,ns,3:mcpool))+1.0e-6)    
            fracmicm  = (miccpool%cpooleq(np,ns,3)+miccpool%cpooleq(np,ns,4))/(sum(miccpool%cpooleq(np,ns,3:mcpool))+1.0e-6)                   
            fraclabm  = miccpool%cpooleq(np,ns,7)/(sum(miccpool%cpooleq(np,ns,3:mcpool))+1.0e-6)  
            write(92,921) micparam%siteid(np),micparam%sorder(np), ns,  &
                          (1000.0*miccpool%cpooleq(np,ns,ip)/micinput%bulkd(np,ns),ip=1,mcpool), &
                          fracpocm,fracmaocm,fracmicm,fraclabm
         enddo

      endif !"pft"
   enddo  !"np"
   totcost = sum(xcost(1:mp))

    deallocate(xcost)
    deallocate(xmod)
    deallocate(xobs7,xmod7,xtop,xbot)

901   format(i6,1x,4(i3,1x),10(f12.4,1x))
921   format(i7,2x,2(i4,2x),20(f12.4,1x))
    end subroutine calcost_hwsd2        
! ##############mesc_cost.f90###########################