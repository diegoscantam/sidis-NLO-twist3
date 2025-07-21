!###############################################################################
!
!   SIDIS CROSS SECTION NUMERICS
!
!   Diego Scantamburlo, 2025
!
!###############################################################################
!   gfortran --std=legacy  sidis.f90 alphaS.f mstwpdf.f fDSS.f fDSS_HESSIAN.f DSSV_gluon_update.f -o sidis.o 
!   ./sidis.o 

! This module declares and initializes some variables
! Useful for avoiding annoying common blocks
module set_variables
    
    implicit none
    public

    ! Declarations
    integer i,j, nplot,nplot2,nplot3,IH,IC,IT,ICpl,IHpl,ITpl,FINI,iset
    parameter(nplot=100000)
    parameter(nplot2=50)
    parameter(nplot3=200)
    character*32 prefix
    real*8 pi, alpha_em, Q2, Q, Sep, weigthedSumPDFFF,&
            zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,zD1c,zD1b,zD1gl,&
            upv,dnv,usea,dsea,s,sbar,chm,cbar,b,bbar,glu, phot

    ! Initialization
    parameter(pi = 3.1415927)
    parameter(alpha_em=1/137.d0)
    parameter(Sep = 100.d0**2.d0) !GeV^2
    !parameter(Q2 = 200. )!GeV^2
    !parameter(Q = SQRT(Q2) )!GeV^2

    !Here we say that the target is a proton iset=0
    parameter(iset = 0)
    parameter(prefix = "mstw2008nlo")
    parameter(FINI = 0)
    ! Here we say that the produced particle is a neutral/charged(IC=0,1,-1) 
    ! hadron (IH=4), pion (IH=1), ...
    parameter(IH = 1)
    parameter(IC = 1)

end module

! This is the main program
program sidis
    
    !Declare and initialize variables in module set_variables
    use set_variables

    ! Declare additional stuff
    character*64 filenamef1,filenameD1, filename
    real*8 dsigmadxdydz, y_arr(0:nplot2),z_arr(0:nplot), x_arr(0:nplot),&
          sigma_of_y(0:nplot) , test, sigmaxz_fixed,&
          x,y,z,muu, sigmaLO_unpolarized!, sigmaNLO_unpolarized
    real*8 output,output2Q,outputQd2,mu,mu_up,mu_down
    real*8 x_arr_plot(3),z_arr_plot(3)
    integer do_this

    EXTERNAL sigmaLO_unpolarized

    ! Initialize the strong coupling constant alphas_S
    ! Later on called with commnand ALPHAS(mu)
    CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)

    !###################### test PDFs and FFs #############################
    ! Write to file f_1(x) and D_1(z), for different flavors
    do_this = 1
    !######################################################################

    ! Photon momentum Q <--> Scale mu for PDFs & FFs
    Q = 10. !GeV
    Q2 = Q*Q
    
    if(do_this.eq.1) then

    ! Define the string which makes up the filename
    write (filenameD1, "(A16,I5,A4)") './output/data/D1', int(Q),'.dat'
    ! and call the subroutine that writes down to file D_1^q(z) @ mu=Q
    call write_D1_to_file(Q,filenameD1)
    ! Do the same for mu=2Q ...
    write (filenameD1, "(A16,I5,A4)") './output/data/D1', int(2.*Q),'.dat'
    call write_D1_to_file(2.*Q,filenameD1)
    ! ... and for mu=Q/2
    write (filenameD1, "(A16,I5,A4)") './output/data/D1', int(Q/2.),'.dat'
    call write_D1_to_file(Q/2.,filenameD1)

    ! Same as above but for the PDF f_1, instead of the FF D_1
    write (filenamef1, "(A16,I5,A4)") './output/data/f1', int(Q),'.dat'
    call write_f1_to_file(Q,filenamef1)
    write (filenamef1, "(A16,I5,A4)") './output/data/f1', int(2.*Q),'.dat'
    call write_f1_to_file(2.*Q,filenamef1)
    write (filenamef1, "(A16,I5,A4)") './output/data/f1', int(Q/2.),'.dat'
    call write_f1_to_file(Q/2.,filenamef1)

    end if

    !###################### alpha_S #######################################
    ! Write to file alpha_S(mu)
    do_this = 0
    !######################################################################

    if(do_this.eq.1) then
    ! Define the string which makes up the filename
    write (filename, "(A25)") './output/data/alpha_S.dat'
    ! and call the subroutine
    call write_alphas_to_file(filename)
    end if
    !###################### dsigma/dxdydz@fixed x&z #######################
    ! Study of 3-differential cross section at fized x & z
    ! Meaning we have a function of 1 variable (y) that we can study
    do_this = 0
    !######################################################################

    !Fixed x & z
    x = 0.01d0
    z = 0.9d0
    
    if(do_this.eq.1) then

    ! Define the string which makes up the filename
    write (filename, "(A23,F4.2,A2,F4.2,A4)") &
          './output/data/sigma_y_x', x,'_z', z,'.dat'
    ! Call appropriate subroutine
    call write_sigma_y_unpolarized_to_file( x, z,filename)

    end if
    !###################### dsigma/dxdydz@fixed y&z #######################
    ! Study of 3-differential cross section at fized y & z
    ! Meaning we have a function of 1 variable (x) that we can study
    do_this = 0
    !######################################################################

    !Fixed y & z
    y = 0.9d0
    z = 0.9d0
    
    if(do_this.eq.1) then

    ! Define the string which makes up the filename
    write (filename, "(A23,F4.2,A2,F4.2,A4)") &
          './output/data/sigma_x_y', y,'_z', z,'.dat'
    ! Call appropriate subroutine
    call write_sigma_x_unpolarized_to_file(y, z,filename)

    end if
    !###################### dsigma/dxdydz@fixed x&y #######################
    ! Study of 3-differential cross section at fized x & y
    ! Meaning we have a function of 1 variable (z) that we can study
    do_this = 0
    !######################################################################

    !Fixed x & y
    x = 0.5d0
    y = 0.9d0
    
    if(do_this.eq.1) then

    ! Define the string which makes up the filename
    write (filename, "(A23,F4.2,A2,F4.2,A4)") &
          './output/data/sigma_z_x', x,'_y', y,'.dat'
    ! Call appropriate subroutine
    call write_sigma_z_unpolarized_to_file(x, y,filename)

    end if
    !###################### dsigma/dxdydz@fixed y #########################
    ! Study of the 2D distribution dsigma/dxdydz@fixed y @ LO
    do_this = 0
    !######################################################################

    ! Fixed value of y
    y=0.1d0

    if(do_this.eq.1) then

    !Open files
    open(4,file='./output/data/sigma_xz.dat',status='unknown')
    !Initialize header of file
    write (4,'(1X)', advance='yes') 

    do i = 0,nplot3
      print *, "Working on sigma(x,z)...   ",int(i),"/",int(nplot3)

      !Update x value
      x_arr(i) =  0.01d0 + dble(i)/dble(nplot3)*(0.98d0 - 0.01d0 )

      do j = 0,nplot3
        !Update z value
        z_arr(j) =  0.01d0 + dble(j)/dble(nplot3)*(0.98d0 - 0.01d0 )

        !Write  sigma(x,y=fixed,z)  down to file
        write(4,'(F30.20,A1)', advance='no') &
                  sigmaLO_unpolarized(x_arr(i),y,z_arr(j)), '&'
      end do

      write (4,'(1X)', advance='yes') 
    end do
    close(4)

    end if
    

end program

!Function that returns the LO unpolarized cross section
function sigmaLO_unpolarized(xB,y,zh)
    use set_variables

    !Usual declarations
    real*8  sigmaLO_unpolarized
    real*8  xB,y,zh
    !COMMON /xyzmu/ xB,y,zh, muu

    !Use relation between variables

    Q2 = Sep * xB * y
    Q = SQRT(Q2)

    !Get PDFs @ specific xB
    call GetAllPDFs(prefix,iset,xB,Q,&
        upv,dnv,usea,dsea,s,sbar,chm,cbar,b,bbar,glu,phot)
    
    !Get D1 FF @ specific zh
    call fDSS(IH,IC,1,zh,Q2,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,&
        zD1c,zD1b,zD1gl)

    !Sum over quark, anti-quarks of e_a^2 * f_1^a * D_1^a
    weigthedSumPDFFF = ((2d0/3d0)**2d0)*( ((upv + usea)/xB)*(zD1u/zh) +&
                                      (usea/xB)*(zD1ub/zh) ) +&
                  ((1d0/3d0)**2d0 )*( ((dnv + dsea)/xB)*(zD1d/zh) +&
                                      (dsea/xB)*(zD1db/zh) ) +&
                  ((1d0/3d0)**2d0) *( (s/xB)*(zD1s/zh) +&
                                      (sbar/xB)*(zD1sb/zh) ) +&
                  ((2d0/3d0)**2d0) *( (chm/xB)*(zD1c/zh) +&
                                      (cbar/xB)*(zD1c/zh) )  +&
                  ((1d0/3d0)**2d0) *( (b/xB)*(zD1b/zh) +&
                                      (bbar/xB)*(zD1b/zh) )

    sigmaLO_unpolarized = (4. * pi * alpha_em * alpha_em /Q2/y) * (1.-y+y*y/2.d0)*&
                          weigthedSumPDFFF

    return 
end function

!Subroutine that gives the NLO unpolarized cross section
!as output
SUBROUTINE sigmaNLO_unpolarized(xB,y,zh,muu,output)
    use set_variables


    !Usual declarations
    !real*8  sigmaNLO_unpolarized
    real*8  xB,y,zh,muu,output
    real*8  xB_,y_,zh_,muu_
    !Pass xyzmu to auxiliary functions
    COMMON /xyzmu/ xB_,y_,zh_, muu_

    real*8  alpha_running, ALPHAS
    real*8  intgrAUXv,intgrAUXv_plus,intgrAUXw,intgrAUXw_plus,intgrAUXwv
    integer ndim,init,ncall,itmx,nprn,idum
    real*8  region_v(2),region_w(2),region_wv(4),&
            region_v_plus(2), region_w_plus(2),&
            region_wv_plus(4), region_w_plus_v(4), sd,chi2a
    real*8  sigmaNLO_unpolarizedAUX,sigmaNLO_unpolarizedAUXv,&
            sigmaNLO_unpolarizedAUXw,sigmaNLO_unpolarizedAUXwv,&
            sigmaNLO_unpolarizedAUXv_plus,sigmaNLO_unpolarizedAUXw_plus

    EXTERNAL ALPHAS, sigmaNLO_unpolarizedAUX,sigmaNLO_unpolarizedAUXv,&
              sigmaNLO_unpolarizedAUXw,sigmaNLO_unpolarizedAUXwv,&
              sigmaNLO_unpolarizedAUXv_plus,sigmaNLO_unpolarizedAUXw_plus

    xB_ = xB
    y_ = y
    zh_ = zh
    muu_ = muu


    !Use relation between variables
    Q2 = Sep * xB * y
    Q = SQRT(Q2)

    !Get alpha_S(mu)
    alpha_running = ALPHAS(muu)

    !Setup integrator
    init=0
    idum = 0
    ncall = 500
    itmx = 5
    nprn = -1 ! Set to 0 for seeing results of integration
    sd = 0. 
    chi2a = 0.d0

    region_v(1) = zh
    region_v(2) = 1.d0
    region_v_plus(1) = 0.d0
    region_v_plus(2) = zh

    region_w(1) = xB
    region_w(2) = 1.d0
    region_w_plus(1) = 0.d0
    region_w_plus(2) = xB

    region_wv(1) = xB
    region_wv(2) = zh
    region_wv(3) = 1.d0
    region_wv(4) = 1.d0


    !Integrals over v & w 
    call vegas(region_v,1,sigmaNLO_unpolarizedAUXv,init,ncall,itmx,nprn,intgrAUXv,sd,chi2a)
    call vegas(region_v_plus,1,sigmaNLO_unpolarizedAUXv_plus,init,ncall,itmx,nprn,intgrAUXv_plus,sd,chi2a)
    call vegas(region_w,1,sigmaNLO_unpolarizedAUXw,init,ncall,itmx,nprn,intgrAUXw,sd,chi2a)
    call vegas(region_w_plus,1,sigmaNLO_unpolarizedAUXw_plus,init,ncall,itmx,nprn,intgrAUXw_plus,sd,chi2a)
    call vegas(region_wv,2,sigmaNLO_unpolarizedAUXwv,init,ncall,itmx,nprn,intgrAUXwv,sd,chi2a)
    

    !print *, sigmaNLO_unpolarizedAUX()
    !print *, intgrAUXv
    !print *, intgrAUXw
    !print *, intgrAUXwv
    output = sigmaNLO_unpolarizedAUX() + intgrAUXv + intgrAUXv_plus&
             + intgrAUXw + intgrAUXw_plus + intgrAUXwv



END SUBROUTINE

!Auxiliary function that calculates sum e_q^2 f_1^q D_1^q
function weigthed_sum_f1D1(xB,zh,muu)
        use set_variables
        real*8 xB,zh,muu,weigthed_sum_f1D1

        !Get PDFs @ specific xB
        call GetAllPDFs(prefix,iset,xB,muu,&
        upv,dnv,usea,dsea,s,sbar,chm,cbar,b,bbar,glu,phot)

        !Get D1 FF @ specific zh
        call fDSS(IH,IC,1,zh,muu*muu,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,&
        zD1c,zD1b,zD1gl)
        
        weigthed_sum_f1D1 =  (((2d0/3d0)**2d0)*( ((upv + usea)/xB)*(zD1u/zh) +&
                                      (usea/xB)*(zD1ub/zh) ) +&
                            ((1d0/3d0)**2d0 )*( ((dnv + dsea)/xB)*(zD1d/zh) +&
                                      (dsea/xB)*(zD1db/zh) ) +&
                            ((1d0/3d0)**2d0) *( (s/xB)*(zD1s/zh) +&
                                      (sbar/xB)*(zD1sb/zh) ) +&
                            ((2d0/3d0)**2d0) *( (chm/xB)*(zD1c/zh) +&
                                      (cbar/xB)*(zD1c/zh) )  +&
                            ((1d0/3d0)**2d0) *( (b/xB)*(zD1b/zh) +&
                                      (bbar/xB)*(zD1b/zh) ))
end function

!Auxiliary function that calculates  [sum e_q^2 f_1^q] D_1^g
function weigthed_sum_f1_times_D1g(xB,zh,muu)
        use set_variables
        real*8 xB,zh,muu,weigthed_sum_f1_times_D1g

        !Get PDFs @ specific xB
        call GetAllPDFs(prefix,iset,xB,muu,&
        upv,dnv,usea,dsea,s,sbar,chm,cbar,b,bbar,glu,phot)

        !Get D1 FF @ specific zh
        call fDSS(IH,IC,1,zh,muu*muu,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,&
        zD1c,zD1b,zD1gl)
        
        weigthed_sum_f1_times_D1g=  (((2d0/3d0)**2d0)*( ((upv + usea)/xB) +&
                                      (usea/xB) ) +&
                            ((1d0/3d0)**2d0 )*( ((dnv + dsea)/xB) +&
                                      (dsea/xB) ) +&
                            ((1d0/3d0)**2d0) *( (s/xB) +&
                                      (sbar/xB) ) +&
                            ((2d0/3d0)**2d0) *( (chm/xB) +&
                                      (cbar/xB) )  +&
                            ((1d0/3d0)**2d0) *( (b/xB) +&
                                      (bbar/xB) ))*zD1gl/zh
end function

!Auxiliary function that calculates f_1^g [sum e_q^2 D_1^q]
function weigthed_sum_D1_times_f1g(xB,zh,muu)
        use set_variables
        real*8 xB,zh,muu,weigthed_sum_D1_times_f1g

        !Get PDFs @ specific xB
        call GetAllPDFs(prefix,iset,xB,muu,&
        upv,dnv,usea,dsea,s,sbar,chm,cbar,b,bbar,glu,phot)

        !Get D1 FF @ specific zh
        call fDSS(IH,IC,1,zh,muu*muu,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,&
        zD1c,zD1b,zD1gl)
        
        weigthed_sum_D1_times_f1g=  (((2d0/3d0)**2d0)*( (zD1u/zh) +&
                                      (zD1ub/zh) ) +&
                            ((1d0/3d0)**2d0 )*( (zD1d/zh) +&
                                      (zD1db/zh) ) +&
                            ((1d0/3d0)**2d0) *( (zD1s/zh) +&
                                      (zD1sb/zh) ) +&
                            ((2d0/3d0)**2d0) *( (zD1c/zh) +&
                                      (zD1c/zh) )  +&
                            ((1d0/3d0)**2d0) *( (zD1b/zh) +&
                                      (zD1b/zh) ))*glu/xB
end function

!Auxiliary unpolarized NLO function
function sigmaNLO_unpolarizedAUX()
        use set_variables
        real*8  sigmaNLO_unpolarizedAUX
        real*8  xB_,y_,zh_,muu_
        real*8  alpha_running, ALPHAS, weigthed_sum_f1D1
        COMMON /xyzmu/ xB_,y_,zh_, muu_        
        EXTERNAL ALPHAS,weigthed_sum_f1D1

        !Use relation between variables
        Q2 = Sep * xB_ * y_
        Q = SQRT(Q2)

        !Get alpha_S(mu)
        alpha_running = ALPHAS(muu_)

        sigmaNLO_unpolarizedAUX= &
                  (4. * pi * alpha_em * alpha_em /Q2/y_) * (1.-y_+y_*y_/2.)*&
                  weigthed_sum_f1D1(xB_,zh_,muu_)*&
                  (1.+ (4./3.)*(-8 -3*LOG(muu_*muu_/Q2))*alpha_running/2./pi)&
                  !Plus distribution 1/1-v 1/1-w no-integral contribution 
                  +(4. * pi * alpha_em * alpha_em /Q2/y_)* (1.-y_+y_*y_/2.)*&
                  (4./3.)*(alpha_running/2.d0/pi)*LOG(1.d0 - zh_)*LOG(1.d0 - xB_)*&
                  weigthed_sum_f1D1(xB_,zh_,muu_)*2.d0  
end function

!Auxiliary unpolarized NLO function
function sigmaNLO_unpolarizedAUXv(v)
        use set_variables
        real*8  sigmaNLO_unpolarizedAUXv
        real*8  xB_,y_,zh_, muu_, v, alpha_running,&
               ALPHAS, weigthed_sum_f1D1, weigthed_sum_f1_times_D1g
        real*8 A
        COMMON /xyzmu/ xB_,y_,zh_, muu_
        EXTERNAL ALPHAS,weigthed_sum_f1D1, weigthed_sum_f1_times_D1g

        !Use relation between variables
        Q2 = Sep * xB_ * y_
        Q = SQRT(Q2)

        !Get alpha_S(mu)
        alpha_running = ALPHAS(muu_)
        
        sigmaNLO_unpolarizedAUXv= &
            (4. * pi * alpha_em * alpha_em /Q2/y_) * &
            (1.-y_+y_*y_/2.)*(4./3.)*(alpha_running/2.d0/pi)*&
            (weigthed_sum_f1D1(xB_,zh_/v,muu_)*(1.d0/v)*&
            (1.d0 - v +(1.d0+v*v)*LOG(1.d0-v)/(1.d0-v) +(1.d0+v*v)*(LOG(v)-LOG(muu_*muu_/Q2))/(1.d0-v))&
             - weigthed_sum_f1D1(xB_,zh_,muu_)*(2.d0*LOG(1.d0-v)/(1.d0-v) +2.d0*(-LOG(muu_*muu_/Q2))/(1.d0-v))& 
            + weigthed_sum_f1_times_D1g(xB_,zh_/v,muu_)*(1.d0/v)*&
            ( (1.d0+(1.d0-v)**2)*(LOG(v*(1.d0-v))-LOG(muu_*muu_/Q2))/v  + v ) )&
            !Plus distribution 1/1-v 1/1-w contribution in single v integral
            +(4. * pi * alpha_em * alpha_em /Q2/y_)* (1.-y_+y_*y_/2.)*&
            (4./3.)*(alpha_running/2.d0/pi)*LOG(1.d0 - xB_)*(&
            weigthed_sum_f1D1(xB_,zh_/v,muu_)*( 1.d0 + v*v) *(1.d0/v)   &
            -weigthed_sum_f1D1(xB_,zh_,muu_)*2.d0  )/(1.d0-v)&
            !Plus distribution 1/1-w contribution, w integral already done (simply a -log(1-xB))
            +(4. * pi * alpha_em * alpha_em /Q2/y_)*&
            (1.-y_+y_*y_/2.)*(4./3.)*(alpha_running/2.d0/pi)*&
            weigthed_sum_f1_times_D1g(xB_,zh_/v,muu_)*LOG(1.d0 - xB_) *&
            (2.d0 -2.d0 * v + v*v)/(v*v)


end function

!Auxiliary unpolarized NLO function
function sigmaNLO_unpolarizedAUXv_plus(v)
        use set_variables
        real*8  sigmaNLO_unpolarizedAUXv_plus
        real*8  xB_,y_,zh_, muu_, v, alpha_running,&
               ALPHAS, weigthed_sum_f1D1, weigthed_sum_f1_times_D1g
        COMMON /xyzmu/ xB_,y_,zh_, muu_
        EXTERNAL ALPHAS,weigthed_sum_f1D1, weigthed_sum_f1_times_D1g

        !Use relation between variables
        Q2 = Sep * xB_ * y_
        Q = SQRT(Q2)

        !Get alpha_S(mu)
        alpha_running = ALPHAS(muu_)
        
        sigmaNLO_unpolarizedAUXv_plus= &
            (4. * pi * alpha_em * alpha_em /Q2/y_) * &
            (1.-y_+y_*y_/2.)*(4./3.)*(alpha_running/2.d0/pi)*&
            weigthed_sum_f1D1(xB_,zh_,muu_)*(-2.d0*LOG(1.d0-v)/(1.d0-v)-2.d0*(-LOG(muu_*muu_/Q2))/(1.d0-v))

end function

!Auxiliary unpolarized NLO function
function sigmaNLO_unpolarizedAUXw(w)
        use set_variables
        real*8  sigmaNLO_unpolarizedAUXw
        real*8  xB_,y_,zh_, muu_, w, alpha_running,&
               ALPHAS, weigthed_sum_f1D1, weigthed_sum_D1_times_f1g
        COMMON /xyzmu/ xB_,y_,zh_, muu_
        EXTERNAL ALPHAS,weigthed_sum_f1D1, weigthed_sum_D1_times_f1g

        !Use relation between variables
        Q2 = Sep * xB_ * y_
        Q = SQRT(Q2)

        !Get alpha_S(mu)
        alpha_running = ALPHAS(muu_)
        

        sigmaNLO_unpolarizedAUXw= &
            ! q -> q
            (4. * pi * alpha_em * alpha_em /Q2/y_) * &
            (1.-y_+y_*y_/2.)*(&
            (4./3.)*(alpha_running/2.d0/pi)*&
            ( weigthed_sum_f1D1(xB_/w,zh_,muu_)*(1.d0/w)*&
            (1.d0 - w +(1.d0+w*w)*LOG(1.d0-w)/(1.d0-w) +(1.d0+w*w)*(-LOG(w)-LOG(muu_*muu_/Q2))/(1.d0-w))&
            ! Plus distribution 1/1-w contribution in the integral
            -weigthed_sum_f1D1(xB_,zh_,muu_)*(2.d0*LOG(1.d0-w)/(1.d0-w) +2.d0*(-LOG(muu_*muu_/Q2))/(1.d0-w)))&
            ! g -> q
            +(1./2.)*(alpha_running/2.d0/pi)*&
            weigthed_sum_D1_times_f1g(xB_/w,zh_,muu_)*&
            ( (w*w+(1.d0-w)**2)*( LOG((1.d0-w)/w)-LOG(muu_*muu_/Q2))  + 2.d0*w*(1.d0-w))  )&
            !Plus distribution 1/1-v 1/1-w contribution in single v integral
            +(4. * pi * alpha_em * alpha_em /Q2/y_)* (1.-y_+y_*y_/2.)*&
            (4./3.)*(alpha_running/2.d0/pi)*LOG(1.d0 - zh_)*(&
            weigthed_sum_f1D1(xB_/w,zh_,muu_)*(+ 1.d0 + w*w) *(1.d0/w)   &
            -weigthed_sum_f1D1(xB_,zh_,muu_)*2.d0  )/(1.d0-w)&
            !Plus distribution 1/1-v contribution, v integral already done (simply a -log(1-zh))
            +(4. * pi * alpha_em * alpha_em /Q2/y_)*(1.-y_+y_*y_/2.)*&
            (1./2.)*(alpha_running/2.d0/pi)*(&
            weigthed_sum_D1_times_f1g(xB_/w,zh_,muu_)*&
            (w*w + (1.d0-w)**2)/w )*LOG(1.d0 - zh_)
            
end function

!Auxiliary unpolarized NLO function
function sigmaNLO_unpolarizedAUXw_plus(w)
        use set_variables
        real*8  sigmaNLO_unpolarizedAUXw_plus
        real*8  xB_,y_,zh_, muu_, w, alpha_running,&
               ALPHAS, weigthed_sum_f1D1, weigthed_sum_f1_times_D1g
        COMMON /xyzmu/ xB_,y_,zh_, muu_
        EXTERNAL ALPHAS,weigthed_sum_f1D1, weigthed_sum_f1_times_D1g

        !Use relation between variables
        Q2 = Sep * xB_ * y_
        Q = SQRT(Q2)

        !Get alpha_S(mu)
        alpha_running = ALPHAS(muu_)
        
        sigmaNLO_unpolarizedAUXw_plus= &
            (4. * pi * alpha_em * alpha_em /Q2/y_) * &
            (1.-y_+y_*y_/2.)*(4./3.)*(alpha_running/2.d0/pi)*&
             weigthed_sum_f1D1(xB_,zh_,muu_)*(-2.d0*LOG(1.d0-w)/(1.d0-w) - 2.d0*(-LOG(muu_*muu_/Q2))/(1.d0-w))

end function

!Auxiliary unpolarized NLO function
function sigmaNLO_unpolarizedAUXwv(Y)
        use set_variables
        real*8  sigmaNLO_unpolarizedAUXwv
        real*8  xB_,y_,zh_, muu_, w,v, Y(*), alpha_running,&
               ALPHAS, weigthed_sum_f1D1, weigthed_sum_f1_times_D1g,&
               weigthed_sum_D1_times_f1g
        COMMON /xyzmu/ xB_,y_,zh_, muu_
        EXTERNAL ALPHAS,weigthed_sum_f1D1, weigthed_sum_f1_times_D1g,&
                  weigthed_sum_D1_times_f1g

        !Use relation between variables
        Q2 = Sep * xB_ * y_
        Q = SQRT(Q2)

        !Get alpha_S(mu)
        alpha_running = ALPHAS(muu_)
        
        w = Y(1)
        v = Y(2)

        sigmaNLO_unpolarizedAUXwv= &
            ! q -> q
            (4. * pi * alpha_em * alpha_em /Q2/y_)*&
            weigthed_sum_f1D1(xB_/w,zh_/v,muu_) *( &
            (1.-y_+y_*y_/2.)*(1.d0/v)*(1.d0/w)*&
            (4./3.)*(alpha_running/2.d0/pi)*&
            (2.*v*v*w*w - 2.*v*v*w - 2.*v*w*w + 4.*v*w &
            + v*v + w*w - 2.*v - 2.*w +2.)/(1.d0-w)/(1.d0-v)&
            +(1.d0-y_)*(1.d0/v)*(1.d0/w)*&
            (4./3.)*(alpha_running/2.d0/pi)*(4.*w*v) )&
            !Plus distribution 1/1-v 1/1-w contribution in double integral
            +(4. * pi * alpha_em * alpha_em /Q2/y_)* (1.-y_+y_*y_/2.)*&
            (4./3.)*(alpha_running/2.d0/pi)*(&
            -weigthed_sum_f1D1(xB_,zh_/v,muu_)*(+ 1.d0 + v*v) *(1.d0/v)   &
            -weigthed_sum_f1D1(xB_/w,zh_,muu_)*(+ 1.d0 + w*w ) *(1.d0/w)   &
            +weigthed_sum_f1D1(xB_,zh_,muu_)*2.d0  )/(1.d0-w)/(1.d0-v)&
            ! g -> q
            +(4. * pi * alpha_em * alpha_em /Q2/y_)*&
            weigthed_sum_D1_times_f1g(xB_/w,zh_/v,muu_) *( &
            (1.-y_+y_*y_/2.)*(1.d0/v)*(1.d0/w)*&
            (1./2.)*(alpha_running/2.d0/pi)*&
            (w*w + (1.d0-w)**2)*(v**2 + (1.d0-v)**2)/(v*(1.d0-v))&
            +(1.d0-y_)*(1.d0/v)*(1.d0/w)*&
            (1./2.)*(alpha_running/2.d0/pi)*(8.*w*(1.d0-w)) )&
            !Plus distribution 1/1-v contribution in the v integral
            +(4. * pi * alpha_em * alpha_em /Q2/y_)*(1.-y_+y_*y_/2.)*&
            (1./2.)*(alpha_running/2.d0/pi)*(&
            -weigthed_sum_D1_times_f1g(xB_/w,zh_,muu_)*&
            (w*w + (1.d0-w)**2)/w )/(1.d0 - v)&
            ! q -> g
            +(4. * pi * alpha_em * alpha_em /Q2/y_)*&
            weigthed_sum_f1_times_D1g(xB_/w,zh_/v,muu_) *( &
            (1.-y_+y_*y_/2.)*(1.d0/v)*(1.d0/w)*&
            (4./3.)*(alpha_running/2.d0/pi)*&
            (1.d0+v*v+w*w-2.*v*w*w-2.*v*v*w+2.*v*v*w*w)/(v*(1.d0-w))&
            +(1.d0-y_)*(1.d0/v)*(1.d0/w)*&
            (4./3.)*(alpha_running/2.d0/pi)*(4.*w*(1.d0-v)) )&
            !Plus distribution 1/1-w contribution in the w integral
            -(4. * pi * alpha_em * alpha_em /Q2/y_)*&
            (1.-y_+y_*y_/2.)*(4./3.)*(alpha_running/2.d0/pi)*&
            weigthed_sum_f1_times_D1g(xB_,zh_/v,muu_) * &
            (2.d0 -2.d0 * v + v*v)/(v*v)/(1.d0-w)
            
end function


!Subroutine that writes to file dsigma/dxdydz
!at fixed x & z. Both LO and NLO unpolarized
SUBROUTINE write_sigma_y_unpolarized_to_file(x,z,filename)
    use set_variables

    real*8 x, z
    character*64 filename
    real*8 output,output2Q,outputQd2
    real*8 y_arr(0:nplot2),sigmaLO_unpolarized
    EXTERNAL sigmaLO_unpolarized

    open(int(x*z*10000),file=filename,status='unknown')
    !Initialize header of file
    write (int(x*z*10000),'(5(A,A1))') 'y', '&', 'sigma','&','sigmaNLO' &
                      ,'&','sigmaNLOmu2Q','&','sigmaNLOmuQd2'  

    do i = 0,nplot2
      print *, "Working on sigma(y) @ NLO...   ",int(i),"/",int(nplot2)

      !Update y value
      y_arr(i) =  0.01d0 + dble(i)/dble(nplot2)*(0.98d0 - 0.001d0 )
      
      call sigmaNLO_unpolarized(x,y_arr(i),z, SQRT(Sep*x*y_arr(i)),output)
      call sigmaNLO_unpolarized(x,y_arr(i),z, 2.d0*SQRT(Sep*x*y_arr(i)),output2Q)
      call sigmaNLO_unpolarized(x,y_arr(i),z, 0.5d0*SQRT(Sep*x*y_arr(i)),outputQd2)
      
      !Write sigma(y) down to file
      write(int(x*z*10000),'(5(F30.20,A1))') y_arr(i), '&', sigmaLO_unpolarized(x,y_arr(i),z),&
                                  '&',output,'&',output2Q,'&',outputQd2
    end do

    close(int(x*z*10000))

END SUBROUTINE

!Subroutine that writes to file dsigma/dxdydz
!at fixed y & z. Both LO and NLO unpolarized
SUBROUTINE write_sigma_x_unpolarized_to_file(y,z,filename)
    use set_variables

    real*8 y, z
    character*64 filename
    real*8 output,output2Q,outputQd2
    real*8 x_arr(0:nplot2),sigmaLO_unpolarized
    EXTERNAL sigmaLO_unpolarized

    open(int(y*z*510000),file=filename,status='unknown')
    !Initialize header of file
    write (int(y*z*510000),'(5(A,A1))') 'x', '&', 'sigma','&','sigmaNLO' &
                      ,'&','sigmaNLOmu2Q','&','sigmaNLOmuQd2'  

    do i = 0,nplot2
      print *, "Working on sigma(x) @ NLO...   ",int(i),"/",int(nplot2)

      !Update x value
      x_arr(i) =  0.01d0 + dble(i)/dble(nplot2)*(0.98d0 - 0.001d0 )
      
      call sigmaNLO_unpolarized(x_arr(i),y,z, SQRT(Sep*x_arr(i)*y),output)
      call sigmaNLO_unpolarized(x_arr(i),y,z, 2.d0*SQRT(Sep*x_arr(i)*y),output2Q)
      call sigmaNLO_unpolarized(x_arr(i),y,z, 0.5d0*SQRT(Sep*x_arr(i)*y),outputQd2)
      
      !Write sigma(x) down to file
      write(int(y*z*510000),'(5(F30.20,A1))') x_arr(i), '&', sigmaLO_unpolarized(x_arr(i),y,z),&
                                  '&',output,'&',output2Q,'&',outputQd2
    end do

    close(int(y*z*510000))

END SUBROUTINE

!Subroutine that writes to file dsigma/dxdydz
!at fixed x & y. Both LO and NLO unpolarized
SUBROUTINE write_sigma_z_unpolarized_to_file(x,y,filename)
    use set_variables

    real*8 x,y 
    character*64 filename
    real*8 output,output2Q,outputQd2
    real*8 z_arr(0:nplot2),sigmaLO_unpolarized
    EXTERNAL sigmaLO_unpolarized

    open(int(x*y*610000),file=filename,status='unknown')
    !Initialize header of file
    write (int(x*y*610000),'(5(A,A1))') 'z', '&', 'sigma','&','sigmaNLO' &
                      ,'&','sigmaNLOmu2Q','&','sigmaNLOmuQd2'  

    do i = 0,nplot2
      print *, "Working on sigma(z) @ NLO...   ",int(i),"/",int(nplot2)

      !Update x value
      z_arr(i) =  0.01d0 + dble(i)/dble(nplot2)*(0.98d0 - 0.001d0 )
      
      call sigmaNLO_unpolarized(x,y,z_arr(i), SQRT(Sep*x*y),output)
      call sigmaNLO_unpolarized(x,y,z_arr(i), 2.d0*SQRT(Sep*x*y),output2Q)
      call sigmaNLO_unpolarized(x,y,z_arr(i), 0.5d0*SQRT(Sep*x*y),outputQd2)
      
      !Write sigma(x) down to file
      write(int(x*y*610000),'(5(F30.20,A1))') z_arr(i), '&', sigmaLO_unpolarized(x,y,z_arr(i)),&
                                  '&',output,'&',output2Q,'&',outputQd2
    end do

    close(int(x*y*610000))

END SUBROUTINE

!Subroutine that writes to file all flavorus PDFS
! f_1(x) given the scale mu
SUBROUTINE write_f1_to_file(muu, filenamef1)
    use set_variables
    real*8 muu
    character*32 filenamef1
    real*8 x_arr(0:nplot)


    open(1,file=filenamef1,status='unknown')
    !Initialize header of files, I put a '&' to separate columns
    write(1,'(13(A,A1))') 'x','&','uv','&','dv','&','usea','&','dsea',&
        '&','s','&','sb','&','c','&','cb','&','b','&','bb','&','gl','&','ph'

    do i=0,nplot
        !Update x value
        x_arr(i) = 0.0001d0 + dble(i)/dble(nplot)*(0.98d0 - 0.0001d0 )

        !Get PDFs @ this x value
        call GetAllPDFs(prefix,iset,x_arr(i),muu,&
            upv,dnv,usea,dsea,s,sbar,chm,cbar,b,bbar,glu,phot)
        
        !Write it down in the file
        write(1,'(13(F16.9,A1))') x_arr(i),'&',upv/dble(x_arr(i)),'&',dnv/dble(x_arr(i)),'&',usea/dble(x_arr(i)),'&',&
            dsea/dble(x_arr(i)),'&',s/dble(x_arr(i)),'&',sbar/dble(x_arr(i)),'&',chm/dble(x_arr(i)),'&',cbar/dble(x_arr(i)),'&',&
            b/dble(x_arr(i)),'&',bbar/dble(x_arr(i)),'&',glu/dble(x_arr(i)),'&',phot/dble(x_arr(i))

    end do
    
    close(1)
    

END SUBROUTINE

!Subroutine that writes to file all flavorus FFS
! D_1(z) given the scale mu
SUBROUTINE write_D1_to_file(muu,filenameD1)
      use set_variables
      real*8 muu
      character*32 filenameD1
      real*8 z_arr(0:nplot)

    open(2,file=filenameD1,status='unknown')
    !Initialize header of files, I put a '&' to separate columns
    write(2,'(10(A,A1))') 'z','&','u','&','d','&','ub','&','db','&',&
        's','&','sb','&','c','&','b','&','gl'


    do i=0,nplot
        !Update z value
        z_arr(i) =  0.0001d0 + dble(i)/dble(nplot)*(0.98d0 - 0.0001d0 )

        !Get D1 @ this z value
        call fDSS(IH,IC,1,z_arr(i),muu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,&
            zD1c,zD1b,zD1gl)
        
        !Write it down in the file
        write(2,'(10(F16.9,A1))') z_arr(i),'&',zD1u/z_arr(i),'&',zD1d/z_arr(i),'&',zD1ub/z_arr(i),'&',&
                zD1db/z_arr(i),'&',zD1s/z_arr(i),'&',zD1sb/z_arr(i),'&',zD1c/z_arr(i),'&',zD1b/z_arr(i),'&',&
                    zD1gl/z_arr(i)
    end do
 
    close(2)
END SUBROUTINE

!Subroutine that writes to file alpha_S(mu)
SUBROUTINE write_alphas_to_file(filename)
    use set_variables

    character*64 filename
    real*8 mu_arr(0:nplot), ALPHAS
    EXTERNAL ALPHAS

    open(76,file=filename,status='unknown')
    !Initialize header of file
    write (76,'(2(A,A1))') 'mu', '&', 'alpha_S'  

    do i = 0,nplot
      print *, "Working on alpha_S(mu)...   ",int(i),"/",int(nplot)

      !Update y value
      mu_arr(i) =  1.d0 + dble(i)/dble(nplot)*(200.d0)
      
      
      !Write alpha_S(mu) down to file
      write(76,'(2(F30.20,A1))') mu_arr(i), '&', ALPHAS(mu_arr(i))
    end do

    close(76)

END SUBROUTINE

!Monte Carlo integrator
SUBROUTINE vegas(region,ndim,fxn,init,ncall,itmx,nprn,tgral,sd,chi2a)
      INTEGER init,itmx,ncall,ndim,nprn,NDMX,MXDIM
      !REAL tgral,chi2a,sd,region(2*ndim),fxn,ALPH,TINY
      REAL*8 tgral,chi2a,sd,region(2*ndim),fxn,ALPH,TINY
      PARAMETER (ALPH=1.5d0,NDMX=50,MXDIM=10,TINY=1.d-30)
      EXTERNAL fxn
      !USE fxn,ran2,rebin
      INTEGER i,idum,it,j,k,mds,nd,ndo,ng,npg,ia(MXDIM),kg(MXDIM)
      !REAL calls,dv2g,dxg,f,f2,f2b,fb,rc,ti,tsi,wgt,xjac,xn,xnd,xo,
      REAL*8 calls,dv2g,dxg,f,f2,f2b,fb,rc,ti,tsi,wgt,xjac,xn,xnd,xo,&
     d(NDMX,MXDIM),di(NDMX,MXDIM),dt(MXDIM),dx(MXDIM),r(NDMX),x(MXDIM),&
     xi(NDMX,MXDIM),xin(NDMX),ran2
      DOUBLE PRECISION schi,si,swgt
      COMMON /ranno/ idum
      SAVE
      if(init.le.0)then
        mds=1
        ndo=1
        do 11 j=1,ndim
          xi(1,j)=1.d0
11      continue
      endif
      if (init.le.1)then
        si=0.d0
        swgt=0.d0
        schi=0.d0
      endif
      if (init.le.2)then
        nd=NDMX
        ng=1
        if(mds.ne.0)then
          ng=(ncall/2.d0+0.25d0)**(1.d0/ndim)
          mds=1
          if((2*ng-NDMX).ge.0)then
            mds=-1
            npg=ng/NDMX+1
            nd=ng/npg
            ng=npg*nd
          endif
        endif
        k=ng**ndim
        npg=max(ncall/k,2)
        calls=npg*k
        dxg=1.d0/ng
        dv2g=(calls*dxg**ndim)**2/npg/npg/(npg-1.d0)
        xnd=nd
        dxg=dxg*xnd
        xjac=1.d0/calls
        do 12 j=1,ndim
          dx(j)=region(j+ndim)-region(j)
          xjac=xjac*dx(j)
12      continue
        if(nd.ne.ndo)then
          do 13 i=1,nd
            r(i)=1.d0
13        continue
          do 14 j=1,ndim
            call rebin(ndo/xnd,nd,r,xin,xi(1,j))
14        continue
          ndo=nd
        endif
        if(nprn.ge.0) write(*,200) ndim,calls,it,itmx,nprn,ALPH,mds,nd,&
     (j,region(j),j,region(j+ndim),j=1,ndim)
      endif
      do 28 it=1,itmx
        ti=0.d0
        tsi=0.d0
        do 16 j=1,ndim
          kg(j)=1
          do 15 i=1,nd
            d(i,j)=0.d0
            di(i,j)=0.d0
15        continue
16      continue
10      continue
          fb=0.d0
          f2b=0.d0
          do 19 k=1,npg
            wgt=xjac
            do 17 j=1,ndim
              xn=(kg(j)-ran2(idum))*dxg+1.d0
              ia(j)=max(min(int(xn),NDMX),1)
              if(ia(j).gt.1)then
                xo=xi(ia(j),j)-xi(ia(j)-1,j)
                rc=xi(ia(j)-1,j)+(xn-ia(j))*xo
              else
                xo=xi(ia(j),j)
                rc=(xn-ia(j))*xo
              endif
              x(j)=region(j)+rc*dx(j)
              wgt=wgt*xo*xnd
17          continue
            f=wgt*fxn(x,wgt)
            f2=f*f
            fb=fb+f
            f2b=f2b+f2
            do 18 j=1,ndim
              di(ia(j),j)=di(ia(j),j)+f
              if(mds.ge.0) d(ia(j),j)=d(ia(j),j)+f2
18          continue
19        continue
          f2b=sqrt(f2b*npg)
          f2b=(f2b-fb)*(f2b+fb)
          if (f2b.le.0.) f2b=TINY
          ti=ti+fb
          tsi=tsi+f2b
          if(mds.lt.0)then
            do 21 j=1,ndim
              d(ia(j),j)=d(ia(j),j)+f2b
21          continue
          endif
        do 22 k=ndim,1,-1
          kg(k)=mod(kg(k),ng)+1
          if(kg(k).ne.1) goto 10
22      continue
        tsi=tsi*dv2g
        wgt=1.d0/tsi
        si=si+dble(wgt)*dble(ti)
        schi=schi+dble(wgt)*dble(ti)**2
        swgt=swgt+dble(wgt)
        tgral=si/swgt
        chi2a=max((schi-si*tgral)/(it-.99d0),0.d0)
        sd=sqrt(1.d0/swgt)
        tsi=sqrt(tsi)
        if(nprn.ge.0)then
          write(*,201) it,ti,tsi,tgral,sd,chi2a
          if(nprn.ne.0)then
            do 23 j=1,ndim
              write(*,202) j,(xi(i,j),di(i,j),i=1+nprn/2,nd,nprn)
23          continue
          endif
        endif
        do 25 j=1,ndim
          xo=d(1,j)
          xn=d(2,j)
          d(1,j)=(xo+xn)/2.d0
          dt(j)=d(1,j)
          do 24 i=2,nd-1
            rc=xo+xn
            xo=xn
            xn=d(i+1,j)
            d(i,j)=(rc+xn)/3.d0
            dt(j)=dt(j)+d(i,j)
24        continue
          d(nd,j)=(xo+xn)/2.d0
          dt(j)=dt(j)+d(nd,j)
25      continue
        do 27 j=1,ndim
          rc=0.d0
          do 26 i=1,nd
            if(d(i,j).lt.TINY) d(i,j)=TINY
            r(i)=((1.-d(i,j)/dt(j))/(dlog(dt(j))-dlog(d(i,j))))**ALPH
            rc=rc+r(i)
26        continue
          call rebin(rc/xnd,nd,r,xin,xi(1,j))
27      continue
28    continue
      return
200   FORMAT(/' input parameters for vegas:  ndim=',i3,'  ncall=',&
     f8.0/28x,'  it=',i5,'  itmx=',i5/28x,'  nprn=',i3,'  alph=',&
     f5.2/28x,'  mds=',i3,'   nd=',i4/(30x,'xl(',i2,')= ',g14.7,' xu(',&
     i2,')= ',g14.7))
201   FORMAT(/' iteration no.',I3,': ','integral =',g14.7,'+/- ',g9.2/&
     ' all iterations: integral =',g14.7,'+/- ',g9.2,' chi**2/it''n ='&
     ,g9.2)
202   FORMAT(/' data for axis ',I2/'    X       delta i       ',&
     '   x       delta i       ','    x       delta i       ',/(1x,&
     f7.5,1x,g11.4,5x,f7.5,1x,g11.4,5x,f7.5,1x,g11.4))
END

!C  (C) Copr. 1986-92 Numerical Recipes Software 0!5,.
     SUBROUTINE rebin(rc,nd,r,xin,xi)
      INTEGER nd
      REAL*8 rc,r(*),xi(*),xin(*)
      INTEGER i,k
      REAL*8 dr,xn,xo
      k=0
      xn=0.d0
      dr=0.d0
      do 11 i=1,nd-1
1       if(rc.gt.dr)then
          k=k+1
          dr=dr+r(k)
          xo=xn
          xn=xi(k)
        goto 1
        endif
        dr=dr-rc
        xin(i)=xn-(xn-xo)*dr/r(k)
11    continue
      do 12 i=1,nd-1
        xi(i)=xin(i)
12    continue
      xi(nd)=1.d0
      return
      END
!C  (C) Copr. 1986-92 Numerical Recipes Software 0!5,.
!c...
!c...
      FUNCTION ran2(idum)
      INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
      REAL*8 ran2,AM,EPS,RNMX
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1.d0/IM1,IMM1=IM1-1,&
      IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791,&
      NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2d-7,RNMX=1.d0-EPS)
      INTEGER idum2,j,k,iv(NTAB),iy
      SAVE iv,iy,idum2
      DATA idum2/123456789/, iv/NTAB*0/, iy/0/
      if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do 11 j=NTAB+8,1,-1
          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          if (idum.lt.0) idum=idum+IM1
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if (idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran2=min(AM*iy,RNMX)
      return
      END
!C  (C) Copr. 1986-92 Numerical Recipes Software 0!5,.

      
