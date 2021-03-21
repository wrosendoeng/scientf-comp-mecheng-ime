MODULE numerical_methods

CONTAINS

    REAL FUNCTION bissec(func(x,f),[k1,k2])
        IMPLICIT NONE
        REAL(8) :: k1,k2,eps,x_avg
        INTEGER :: i, imax

        eps = 1.e-3
        imax = 100

        DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
            IF ((k1-k2)/2 .LT. eps .OR. polynomial(x_avg,f) .LT. eps) THEN
                x_avg = (k2-k1)/2
                stop
            ELSE
                x_avg = (k2-k1)/2
                M = polynomial(k1,f)
                N = polynomial(x_avg,f)
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