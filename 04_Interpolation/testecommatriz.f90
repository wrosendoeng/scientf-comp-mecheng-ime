program name
    implicit none
    real(8) :: a(2), g(3)

    g = (/1.0d0,2.0d0,3.0d0/)

    a = (g(2:3)-g(1:2))/dble(6);
    
    write(*,*) a
end program name