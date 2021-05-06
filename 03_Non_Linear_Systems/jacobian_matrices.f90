module jacobian_matrices
    use fgsl
    use functions
    implicit none 
    contains

    function jacobian_rosembrock(x) result(jrosemb) ! Rosembrock Jacobian and its inverse
        implicit none 
        real(fgsl_double) :: x(2)
        real(fgsl_double) :: jrosemb(2,2)
    
        jrosemb(1,1) = -20_fgsl_double*x(1)
        jrosemb(2,1) = -1.0_fgsl_double
        jrosemb(1,2) = 10.0_fgsl_double
        jrosemb(2,2) = 0.0_fgsl_double
        
    end function jacobian_rosembrock

    function jacobian_tribroyden(x,n) result(jtribro) ! Tridiagonal Broyden Jacobian and its inverse
        implicit none
        integer(fgsl_int) :: i, j, n
        real(fgsl_double) :: x(n)
        real(fgsl_double) :: jtribro(n,n)
        
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

        character(kind=fgsl_char,len=10) :: func ! Rosembrock or Broyden
        integer(fgsl_int) :: i, n
        real(fgsl_double) :: h, x(n), xdelta(n)
        real(fgsl_double) :: jgeneral(n,n)
        
        h = 1.e-10
        xdelta = x
        
        select case(func)
        case('2d') ! rosembrock function
            do i = 1, n
                xdelta(i) = xdelta(i) + h
                jgeneral(:,i) = (rosembrock(xdelta) - rosembrock(x))/h
                xdelta(i) = x(i)
            end do
        case('2e') ! broyden function
            do i = 1, n
                xdelta(i) = xdelta(i) + h
                jgeneral(:,i) = (tridiagonal_broyden(xdelta,n) - tridiagonal_broyden(x,n))/h
                xdelta(i) = x(i)
            end do 
        end select

    end function
   
end module jacobian_matrices
