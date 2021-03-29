!GAUSS-ELIMINATION MATRIX METHOD
!Transform linear system into an equivalent linear system with superior triangular shape

MODULE gausselimination

    DO (k = 1,n-1)
        DO (i = k+1, n)
            m = a(i,k)/a(k,k)
            a(i,k) = 0
            DO (j = k+1, n)
                a(i,j) = a(i,j) - m*a(k,j)
                b(i) = b(i) - m*b(k)
            END DO
        END DO
    END DO

END MODULE gausselimination