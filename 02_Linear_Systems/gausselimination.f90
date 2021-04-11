!GAUSS-ELIMINATION MATRIX METHOD
!Transform linear system into an equivalent linear system with superior triangular shape

MODULE direct_resolution_methods

    implicit NONE
    integer(4) :: i, j, k, n

    subroutine gausselimination()

        DO (k = 1,n-1)
            DO (i = k+1, n)
                m = a(i,k)/a(k,k)
                a(i,k) = 0
                DO j = k+1, n
                    a(i,j) = a(i,j) - m*a(k,j)
                    b(i) = b(i) - m*b(k)
                END DO
            END DO
        END DO

        ! Calculate triangular-superior matrix
        DO (k = n-1,1) THEN
            s = 0
            DO j = (k+1,n)
                s = s + a(k,j)*x(j)
                x(k) = (b(k)-s)/a(k,k)
            END DO
        END DO

    end subroutine

    subroutine lu_factoration() ! LU Factoration with Parcial Pivot
        
        ! Solving c = Pb
        do i = 1,n 
            r = p(i)
            c(i) = b(r)
        end do

        ! Solving Ly = c
        do i = 1,n 
            sumy = 0
            do j = 1, i-1
                sumy = sumy + a(i,j)*y(j)
            end do
            y(i) = c(i) - sumy
        end do

        ! Solving Ux = y
        do i = n,1
            sumx = 0
            do j = (i+1),n 
                sumx = sumx + a(i,j)*x(j)
            end do
            x(i) = (y(i) - sumx)/a(i,i)
        end do

    end subroutine 

END MODULE direct_resolution_methods