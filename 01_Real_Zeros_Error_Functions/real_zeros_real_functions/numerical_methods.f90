MODULE numerical_methods

CONTAINS

    FUNCTION bissec(f,k1,k2)
        IMPLICIT NONE
        REAL(8) :: eps,x_avg,f,M,N,k1,k2,bissec
        INTEGER :: i, imax

        eps = 1.e-3
        imax = 100

        DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
            x_avg = (k1-k2)/2
            IF (x_avg .LT. eps .OR. f(x_avg) .LT. eps) THEN
                PRINT *, "The value of x_avg: ", x_avg
                PRINT *, "The value of f(x_avg): ",f(x_avg)
                stop
            ELSE
                M = f(k1)
                N = f(x_avg)
                IF (M*N .GT. 0) THEN
                    x_avg = k1
                ELSE
                    x_avg = k2
                END IF
            END IF
            i = i+1
        END DO

    END FUNCTION

END MODULE
