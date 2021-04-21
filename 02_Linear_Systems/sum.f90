<<<<<<< HEAD
program sum
    implicit NONE
    integer(4) :: i, j
    real(8) :: v(3,3) = reshape((/1.0,4.0,7.0,2.0,5.0,8.0,3.0,6.0,9.0/), shape(v))
    real(8) :: auxiliar(3) = reshape((/1.0,5.0,9.0/), shape(auxiliar))
    real(8) :: result(3) = reshape((/0.0,0.0,0.0/), shape(result))
    real(8) :: factor(3) = reshape((/0.0,0.0,0.0/), shape(factor))

    do i = 1, 3
        do j = 1, 3
            result(i) = v(i,j)*factor(j) + auxiliar(i)
        end do
    end do
    
    do i = 1, 3
        print *, v(i,:), result(i), auxiliar(i)
    end do

end program sum

! 1 2 3     0    1       (1*0+2*0+3*0)+1 = 1  
! 4 5 6  x  0 +  5 ====> (4*0+5*0+6*0)+5 = 5
! 7 8 9     0    9       (7*0+8*0+9*0)+9 = 9

! a11 a12 a13
! a21 a22 a23
! a31 a32 a33

! result(1) = a11 + a12 + a13 
! result(2) = a21 + a22 + a23 
! result(3) = a31 + a32 + a33 


=======
program sum
    implicit NONE
    integer(4) :: i, j
    real(8) :: v(3,3) = reshape((/1.0,4.0,7.0,2.0,5.0,8.0,3.0,6.0,9.0/), shape(v))
    real(8) :: auxiliar(3) = reshape((/1.0,5.0,9.0/), shape(auxiliar))
    real(8) :: result(3) = reshape((/0.0,0.0,0.0/), shape(result))
    real(8) :: factor(3) = reshape((/0.0,0.0,0.0/), shape(factor))

    do i = 1, 3
        do j = 1, 3
            result(i) = v(i,j)*factor(j) + auxiliar(i)
        end do
    end do
    
    do i = 1, 3
        print *, v(i,:), result(i), auxiliar(i)
    end do

end program sum

! 1 2 3     0    1       (1*0+2*0+3*0)+1 = 1  
! 4 5 6  x  0 +  5 ====> (4*0+5*0+6*0)+5 = 5
! 7 8 9     0    9       (7*0+8*0+9*0)+9 = 9

! a11 a12 a13
! a21 a22 a23
! a31 a32 a33

! result(1) = a11 + a12 + a13 
! result(2) = a21 + a22 + a23 
! result(3) = a31 + a32 + a33 


>>>>>>> 2bd4f61f96afd92a8f1564a188da886181568064
