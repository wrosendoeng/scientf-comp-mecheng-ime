program teste_idiota

    implicit none

    integer(4) :: i
    integer(4), parameter :: PI = 3.14
    real(8) :: x, y(3)

    x = 2.0d0 
    y = 5.0d0

    do i = 1, 10, 1
        write(*,*) x, y, 1.1343d4**2
    end do

end program teste_idiota