program lagrange_interpolation
    use parameters, only: rp => double, ip => int 
    implicit none

    integer(ip) :: i, j, n_points=3
    real(rp) :: x_int, y_int, p
    real(rp), allocatable :: x(:), y(:)
    
    allocate(x(n_points), y(n_points))
    
    do i = 1, n_points
        x(i) = 3.0+(i-1)*0.2
    end do
    
    y = exp(x)
    x_int = 3.1
    y_int = 0.0

    do i = 1, n_points
        p = 1.0
        do j = 1, n_points
            if (i .ne. j) then 
                p = p * (x_int - x(j))/(x(i) - x(j))
            end if
        end do
        y_int = y_int + p * y(i)
    end do
    
    print('(f4.2,A10,f5.2)'), x_int," ",y_int
    deallocate(x,y)

end program 