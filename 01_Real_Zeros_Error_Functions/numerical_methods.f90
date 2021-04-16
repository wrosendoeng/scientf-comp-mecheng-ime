<<<<<<< HEAD
MODULE numerical_methods
IMPLICIT NONE
REAL(8) :: eps1 = 5.e-4, eps2 = 5.e-4
INTEGER :: i = 1, imax = 100

CONTAINS
    FUNCTION bissec(f,x0,x1)
        IMPLICIT NONE
        REAL(8) :: x,x0,x1,f,y0,y1,bissec

        DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
            x = (x1+x0)/2
            IF ((x1-x0) .LT. eps1) THEN
                PRINT '(A32,E25.17)', "The value of x: ", x/1.e-3
                PRINT '(A32,E25.17)', "The value of f(x): ", f(x)/1.e-3
                PRINT *, "How many iterations were used in bisection method: ", i
                stop
            ELSE
                y0 = f(x0)
                y1 = f(x)
                IF (y0*y1 .GT. 0) THEN
                    x0 = x
                ELSE
                    x1 = x
                END IF
            END IF
            i = i+1
        END DO
    END FUNCTION

    FUNCTION falsepos(f,x0,x1)
        IMPLICIT NONE
        REAL(8) :: x,x0,x1,f,y0,y1,falsepos

        DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
            x = (x0*f(x1)-x1*f(x0))/(f(x1)-f(x0))
            IF ((x1-x0) .LT. eps1 .OR. ABS(f(x0)) < eps2 .OR. ABS(f(x1)) < eps2) THEN
                PRINT '(A32,E25.17)', "The value of x: ", x/1.e-3
                PRINT '(A32,E25.17)', "The value of f(x): ", f(x)/1.e-3
                PRINT *, "How many iterations were used in false position method: ", i
                STOP
            ELSE
                y0 = f(x0)
                y1 = f(x)
                IF (ABS(y1) .LT. eps2) THEN
                    PRINT '(A32,E25.17)', "The value of x: ", x/1.e-3
                    PRINT '(A32,E25.17)', "The value of f(x): ", y1/1.e-3
                    PRINT *, "How many iterations were used in false position method: ", i
                    STOP  
                ELSE IF (y0*y1 .GT. 0) THEN
                    x0 = x
                ELSE
                    x1 = x
                END IF
            END IF
            i = i+1
        END DO

    END FUNCTION

    FUNCTION newton_raphson(f,nf,x0,x1)
        IMPLICIT NONE 
        REAL(8) :: f, nf, x0, x1, newton_raphson

        DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
            IF (ABS(f(x0)) .LT. eps1) THEN
                PRINT '(A32,E25.17)', "The value of x: ", x0/1.e-3
                PRINT '(A32,E25.17)', "The value of f(x): ", f(x0)/1.e-3
                PRINT *, "How many iterations were used in Secant method: ", i
                STOP
            ELSE
                x1 = x0 - f(x0)/nf(f,x0)
                IF (ABS(f(x1)) .LT. eps1 .OR. ABS(x1-x0) .LT. eps2) THEN
                    PRINT '(A32,E25.17)', "The value of x: ", x1/1.e-3
                    PRINT '(A32,E25.17)', "The value of f(x): ", f(x1)/1.e-3
                    PRINT *, "How many iterations were used in Secant method: ", i
                    STOP
                ELSE
                    x0 = x1
                END IF 
            END IF
            i = i+1
        END DO

    END FUNCTION

    FUNCTION secant(f,x0,x1)
        IMPLICIT NONE 
        REAL(8) :: f, x0, x1, x2, secant

        DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
            IF (ABS(f(x0)) .LT. eps1) THEN
                PRINT '(A32,E25.17)', "The value of x: ", x0/1.e-3
                PRINT '(A32,E25.17)', "The value of f(x): ", f(x0)/1.e-3
                PRINT *, "How many iterations were used in secant method: ", i
                STOP
            ELSE IF (ABS(f(x1)) .LT. eps1 .OR. ABS(x1-x0) .LT. eps2) THEN
                PRINT '(A32,E25.17)', "The value of x: ", x1/1.e-3
                PRINT '(A32,E25.17)', "The value of f(x): ", f(x1)/1.e-3
                PRINT *, "How many iterations were used in secant method: ", i
                STOP
            ELSE
                x2 = x1 - f(x1)/(f(x1)-f(x0))*(x1-x0)
                IF (ABS(f(x2)) .LT. eps1 .OR. ABS(x2-x1) .LT. eps2) THEN
                    PRINT '(A32,E25.17)', "The value of x: ", x2/1.e-3
                    PRINT '(A32,E25.17)', "The value of f(x): ", f(x2)/1.e-3
                    PRINT *, "How many iterations were used in Secant method: ", i
                    STOP
                ELSE
                    x0 = x1
                    x1 = x2
                END IF 
            END IF
            i = i+1
        END DO

    END FUNCTION

=======
MODULE numerical_methods
IMPLICIT NONE
REAL(8) :: eps1 = 5.e-4, eps2 = 5.e-4
INTEGER :: i = 1, imax = 100

CONTAINS
    FUNCTION bissec(f,x0,x1)
        IMPLICIT NONE
        REAL(8) :: x,x0,x1,f,y0,y1,bissec

        DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
            x = (x1+x0)/2
            IF ((x1-x0) .LT. eps1) THEN
                PRINT '(A32,E25.17)', "The value of x: ", x/1.e-3
                PRINT '(A32,E25.17)', "The value of f(x): ", f(x)/1.e-3
                PRINT *, "How many iterations were used in bisection method: ", i
                stop
            ELSE
                y0 = f(x0)
                y1 = f(x)
                IF (y0*y1 .GT. 0) THEN
                    x0 = x
                ELSE
                    x1 = x
                END IF
            END IF
            i = i+1
        END DO
    END FUNCTION

    FUNCTION falsepos(f,x0,x1)
        IMPLICIT NONE
        REAL(8) :: x,x0,x1,f,y0,y1,falsepos

        DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
            x = (x0*f(x1)-x1*f(x0))/(f(x1)-f(x0))
            IF ((x1-x0) .LT. eps1 .OR. ABS(f(x0)) < eps2 .OR. ABS(f(x1)) < eps2) THEN
                PRINT '(A32,E25.17)', "The value of x: ", x/1.e-3
                PRINT '(A32,E25.17)', "The value of f(x): ", f(x)/1.e-3
                PRINT *, "How many iterations were used in false position method: ", i
                STOP
            ELSE
                y0 = f(x0)
                y1 = f(x)
                IF (ABS(y1) .LT. eps2) THEN
                    PRINT '(A32,E25.17)', "The value of x: ", x/1.e-3
                    PRINT '(A32,E25.17)', "The value of f(x): ", y1/1.e-3
                    PRINT *, "How many iterations were used in false position method: ", i
                    STOP  
                ELSE IF (y0*y1 .GT. 0) THEN
                    x0 = x
                ELSE
                    x1 = x
                END IF
            END IF
            i = i+1
        END DO

    END FUNCTION

    FUNCTION newton_raphson(f,nf,x0,x1)
        IMPLICIT NONE 
        REAL(8) :: f, nf, x0, x1, newton_raphson

        DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
            IF (ABS(f(x0)) .LT. eps1) THEN
                PRINT '(A32,E25.17)', "The value of x: ", x0/1.e-3
                PRINT '(A32,E25.17)', "The value of f(x): ", f(x0)/1.e-3
                PRINT *, "How many iterations were used in Secant method: ", i
                STOP
            ELSE
                x1 = x0 - f(x0)/nf(f,x0)
                IF (ABS(f(x1)) .LT. eps1 .OR. ABS(x1-x0) .LT. eps2) THEN
                    PRINT '(A32,E25.17)', "The value of x: ", x1/1.e-3
                    PRINT '(A32,E25.17)', "The value of f(x): ", f(x1)/1.e-3
                    PRINT *, "How many iterations were used in Secant method: ", i
                    STOP
                ELSE
                    x0 = x1
                END IF 
            END IF
            i = i+1
        END DO

    END FUNCTION

    FUNCTION secant(f,x0,x1)
        IMPLICIT NONE 
        REAL(8) :: f, x0, x1, x2, secant

        DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
            IF (ABS(f(x0)) .LT. eps1) THEN
                PRINT '(A32,E25.17)', "The value of x: ", x0/1.e-3
                PRINT '(A32,E25.17)', "The value of f(x): ", f(x0)/1.e-3
                PRINT *, "How many iterations were used in secant method: ", i
                STOP
            ELSE IF (ABS(f(x1)) .LT. eps1 .OR. ABS(x1-x0) .LT. eps2) THEN
                PRINT '(A32,E25.17)', "The value of x: ", x1/1.e-3
                PRINT '(A32,E25.17)', "The value of f(x): ", f(x1)/1.e-3
                PRINT *, "How many iterations were used in secant method: ", i
                STOP
            ELSE
                x2 = x1 - f(x1)/(f(x1)-f(x0))*(x1-x0)
                IF (ABS(f(x2)) .LT. eps1 .OR. ABS(x2-x1) .LT. eps2) THEN
                    PRINT '(A32,E25.17)', "The value of x: ", x2/1.e-3
                    PRINT '(A32,E25.17)', "The value of f(x): ", f(x2)/1.e-3
                    PRINT *, "How many iterations were used in Secant method: ", i
                    STOP
                ELSE
                    x0 = x1
                    x1 = x2
                END IF 
            END IF
            i = i+1
        END DO

    END FUNCTION

>>>>>>> 2bd4f61f96afd92a8f1564a188da886181568064
END MODULE