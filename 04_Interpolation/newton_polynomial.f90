program newton_polynomial

    integer(4) :: n_points=3, i, k
    real(4) :: x_int, y_int 
    real(4), dimension(:), allocatable :: x, y, divided_diff

    allocate(x(n_points), y(n_points), divided_diff(n_points))

    x = (/1.1d0, 2.2d0, 3.5d0 /)
    y = (/10.0d0, 29.0d0, 90.0d0/)
    x_int = 1.5d0

    do i = 1, n_points
        divided_diff(i) = y(i)
    end do 

    do k = 1, n_points-1
        do i = n_points, k + 1, -1
            divided_diff(i) = (divided_diff(i)-divided_diff(i-1))/(x(i)-x(i-k))
        end do
    end do

    y_int = divided_diff(n_points)

    do i = n_points-1, 1, -1
        y_int = y_int*(x_int-x(i))+divided_diff(i)
    end do

    print *, x_int, y_int
    deallocate(x, y, divided_diff)
end program