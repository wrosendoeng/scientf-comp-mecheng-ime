module functions
    use fgsl
    implicit none 
    contains

    function rosembrock(x) result(ros)
        implicit none
        real(fgsl_double),intent(in) :: x(2)
        real(fgsl_double)            :: ros(2)
    
        ros(1) = 10.0_fgsl_double*(x(2)-x(1)**(2))
        ros(2) = 1 - x(1)
    
    end function rosembrock

    function tridiagonal_broyden(x,n) result(trigbroy)
        implicit none
        integer(fgsl_int)            :: i, n
        real(fgsl_double),intent(in) :: x(n)
        real(fgsl_double)            :: trigbroy(n)

        do i = 1, n
            if (i == 1) then
                trigbroy(i) = (3-2*x(i))*x(i) - 2*x(i+1) + 1
            else if (i .ge. 2 .and. i .le. n-1) then
                trigbroy(i) = (3-2*x(i))*x(i) - x(i-1) - 2*x(i+1) + 1
            else
                trigbroy(i) = (3-2*x(i))*x(i) - x(i-1) + 1
            end if
        end do

    end function

end module functions