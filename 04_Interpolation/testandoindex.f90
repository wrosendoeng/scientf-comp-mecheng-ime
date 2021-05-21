program principal
    implicit none
    integer(4) :: i, j
    integer(8) :: a(4,2), x(4), y(4)

    x = (/1, 2, 3, 4/)
    y = (/3,5,7,9/)

    a(:,1) = x 
    a(:,2) = y 

    do i = 1,4
        print *, a(i,:)
    end do

end program principal