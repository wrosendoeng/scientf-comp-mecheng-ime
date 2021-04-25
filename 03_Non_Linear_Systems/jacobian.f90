module jacobian 

    function jacobian_rosembrock(x) result(jrosemb) ! Rosembrock Jacobian and its inverse
        implicit none 
        real(fgsl_double),intent(in)  :: x(2)
        real(fgsl_double),intent(out) :: jrosemb(2,2)
    
        jrosemb(1,1) = -20_fgsl_double*x(1)
        jrosemb(2,1) = -1.0_fgsl_double
        jrosemb(1,2) = 10.0_fgsl_double
        jrosemb(2,2) = 0.0_fgsl_double
        
    end function jacobian_rosembrock

    function jacobian_tribroyden(x, n) result(jtribro) ! Tridiagonal Broyden Jacobian and its inverse
        implicit none
        integer(fgsl_int)  :: i, j, n
        real(fgsl_double),intent(in)  :: x(n)
        real(fgsl_double),intent(out) :: jac(n,n)
        
        do j = 1, n
            do i = 1, n
                if (i == j) then
                    jtribro(i,j) = -4.0_fgsl_double*x(i) + 3.0_fgsl_double
                else if (i == j + 1) then
                    jtribro(i,j) = -1.0_fgsl_double
                else if (j == i + 1) then
                    jtribro(i,j) = -2.0_fgsl_double
                else
                    jtribro(i,j) = 0.0_fgsl_double
                end if
            end do
        end do
    
    end function

    function general_jacobian(x,func,n) result(jgeneral) ! Approximate Jacobian using Finite-Differences

        character(kind=fgsl_char,len=10) :: func
        integer(fgsl_int) :: n, e(n)
        real(fgsl_double) :: h = 0.1_fgsl_double
        real(fgsl_double), intent(in) :: x(n)
      
        do j = 1, n
            e = 0.0_fgsl_int 
            e(j) = 1.0_fgsl_int
            y1 = y(x + h*e)
            y0 = y(x - h*e)
            jgeneral(:,j) = (y1 - y0)/(2.0_fgsl_double*h)
        end do

    end function
    
end module jacobian