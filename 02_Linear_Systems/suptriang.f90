!RESOLUTION OF A SUPERIOR TRIANGULAR SYSTEM
!Given a sup. triangular n x n with non-zeros elements in main matrix diagonal x_n, x_n-1,...,x_1 

MODULE suptriangular

    xn = b(n)/a(n,n) 

    DO (k = n-1,1) THEN
        s = 0
        DO j = (k+1,n)
            s = s + a(k,j)*x(j)
            x(k) = (b(k)-s)/a(k,k)
        END DO
    END DO


END MODULE suptriangular