program name
    implicit none
    integer(4) :: iunit, iter, ioserror
    real(8), dimension(30,19) :: aero_coef!Mach, cx0, cx2, cd2, cx4, cla, cna, &
    !cna3, cmag_f, cma, cma3, cma5, cmq, cmq2, cspin, cld, cxf, cxb, cpn

    open(newunit=iunit,status='old',file='prodas.csv', iostat= ioserror,&
        action='read',form='formatted',access='sequential',position='rewind')         

    read(iunit,*)
    do iter = 1, 30, 1
        read(iunit,*) aero_coef(iter,:)
        print ('(f25.10*)'), aero_coef(iter,:)
    end do

    close(iunit)
    
end program name