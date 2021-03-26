MODULE numerical_methods
IMPLICIT NONE
real(8) :: eps1 = 1.e-3, eps2 = 1.e-3
integer :: i = 1, imax = 100

CONTAINS
  FUNCTION bissec(f,a,b)
    IMPLICIT NONE
    REAL(8) :: x,f,Y1,Y2,a,b,bissec

    DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
        x = (b+a)/2
        IF ((b-a) .LT. eps1) THEN
            PRINT '(A32,E25.17)', "The value of x: ", x
            PRINT '(A32,E25.17)', "The value of f(x): ", f(x)
            PRINT *, "How many iterations were used in bisection method: ", i
            stop
        ELSE
            Y1 = f(a)
            Y2 = f(x)
            IF (Y1*Y2 .GT. 0) THEN
                a = x
            ELSE
                b = x
            END IF
        END IF
        i = i+1
    END DO

  END FUNCTION

  FUNCTION falsepos(f,a,b)
    implicit NONE
    real(8) :: x,f,Y1,Y2,a,b,falsepos

    DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
        x = (a*f(b)+b*f(a))/(f(b)-f(a))
        IF ((b-a) .LT. eps1 .OR. ABS(f(a)) < eps2 .OR. ABS(f(b)) < eps2) THEN
            PRINT '(A32,E25.17)', "The value of x: ", x
            PRINT '(A32,E25.17)', "The value of f(x): ", f(x)
            PRINT *, "How many iterations were used in false position method: ", i
            stop
        ELSE
            Y1 = f(a)
            Y2 = f(x)
            IF (ABS(Y2) .LT. eps2) THEN
                PRINT '(A32,E25.17)', "The value of x: ", x
                PRINT '(A32,E25.17)', "The value of f(x): ", f(x)
                PRINT *, "How many iterations were used in false position method: ", i
                stop  
            ELSE IF (Y1*Y2 .GT. 0) THEN
                a = x
            ELSE
                b = x
            END IF
        END IF
        i = i+1
    END DO

END FUNCTION

function newton_raphson(f,a,b)
    implicit none
    real(8) :: x,f,Y1,Y2,a,b,newton_raphson

    DO WHILE (i .LE. imax) ! 100 iterations to prevent infinite loop
        IF (ABS(f(a)) .LT. eps1 .OR. ABS(f(a)) < eps2 .OR. ABS(f(b)) < eps2) THEN
            PRINT '(A32,E25.17)', "The value of x: ", x
            PRINT '(A32,E25.17)', "The value of f(x): ", f(x)
            PRINT *, "How many iterations were used in false position method: ", i
            stop
        ELSE
            Y1 = f(a)
            Y2 = f(x)
            IF (Y2 .LT. eps2) THEN
                x
                stop
            ELSE IF (Y1*Y2 .GT. 0) THEN
                a = x
            ELSE
                b = x
            END IF
        END IF
        i = i+1
    END DO

end function

END MODULE
