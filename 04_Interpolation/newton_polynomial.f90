program newton_polynomial ! Forma de Newton

    integer(4) :: n_points=5, i, k
    real(8) :: x_int, y_int, error_int, divided_error
    real(8), dimension(:), allocatable :: x, y, divided_diff

    allocate(x(n_points), y(n_points), divided_diff(n_points))

    do i = 1, n_points
        x(i) = real(i-1,kind=4)*0.5
    end do

    y = (/-2.241, -1.65, -0.594, 1.34, 4.564/) !(/-2.78, -2.241, -1.65, -0.594, 1.34, 4.564/)
    x_int = 1.23d0

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

    error_int = 1.0
    do i = 1, n_points-1, 1
        error_int = error_int*(x_int-x(i))
    end do

    error_int = error_int * divided_diff(n_points)
    ! print *, x_int, y_int, error_int
    print('(e25.10)'), x_int, y_int, error_int
    deallocate(x, y, divided_diff)
end program