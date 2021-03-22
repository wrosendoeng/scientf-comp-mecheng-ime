MODULE methods
IMPLICIT NONE
CONTAINS
  FUNCTION bissec(f,a,b)
    IMPLICIT NONE
    REAL(8) :: eps,x_avg,f,Y1,Y2,a,b,bissec
    INTEGER :: i, imax

    eps = 1.e-3
    imax = 100

    DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
        x_avg = (b+a)/2
        IF ((b-a) .LT. eps) THEN
            PRINT *, "The value of x_avg: ",x_avg
            PRINT *, "The value of f(x_avg): ",f(x_avg)          
            PRINT *, "How many iterations were used: ", i
            stop
        ELSE
            Y1 = f(a)
            Y2 = f(x_avg)
            IF (Y1*Y2 .GT. 0) THEN
                a = x_avg
            ELSE
                b = x_avg
            END IF
        END IF
        i = i+1
    END DO

  END FUNCTION

END MODULE
