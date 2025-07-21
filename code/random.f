!###################### dsigma/dxdz study ##################################
    
    !Calculate integral over y of y dependent part of sigma
    region(0) = 0.0001
    region(1) = 1.0
    
    init=0
    idum = 0
    ncall=1000000
    itmx=5
    nprn=-1 ! Set to 0 for seeing results of integration
    sd =0.d0
    chi2a =0.d0

    call vegas(region,ndim,sigma0_y,init,ncall,itmx,nprn,output_vegas,sd,chi2a)
  
    !Open files
    open(0,file='./output/sigma_xz.dat',status='unknown')
    !Initialize header of file
    write (0,'(1X)', advance='yes') 

    do i = 0,nplot
      !Update x value
      x_arr(i) =  0.0001d0 + dble(i)/dble(nplot)*(0.98d0 - 0.0001d0 )

      do j = 0,nplot
        !Update z value
        z_arr(j) =  0.0001d0 + dble(j)/dble(nplot)*(0.98d0 - 0.0001d0 )

        !Write integral dy [ sigma(x,y,z) ] down to file
        write(0,'(F30.20,A1)', advance='no') output_vegas*sigma0_xz(x_arr(i),z_arr(j)), '&'
      end do

      write (0,'(1X)', advance='yes') 
    end do
    close(0)
    
    !###################### dsigma/dy study ##################################
    
    !Calculate integral over x and z
    region2(0) = 0.0001
    region2(1) = 0.0001
    region2(2) = 1.0
    region2(3) = 1.0

    init=0
    idum = 0
    ncall=100000
    itmx=30
    nprn=-1 ! Set to 0 for seeing results of integration
    sd =0.d0
    chi2a =0.d0
    
    call vegas(region2,ndim2,sigma0_xz,init,ncall,itmx,nprn,output_vegas2,sd,chi2a)
  
    !Open files
    open(3,file='./output/sigma_y.dat',status='unknown')
    !Initialize header of file
    write (3,'(2(A,A1))') 'y', '&', 'sigma' 

    do i = 0,nplot
      !Update y value
      y_arr(i) =  0.0001d0 + dble(i)/dble(nplot)*(0.98d0 - 0.0001d0 )

      !Write sigma(y) down to file
      write(3,'(2(F30.20,A1))') y_arr(i), '&', output_vegas2*sigma0_y(y_arr(i))
    end do

    close(3)