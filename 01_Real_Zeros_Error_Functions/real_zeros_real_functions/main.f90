PROGRAM main

    !Declaring precompiled module named "numerical methods"
    use numerical_methods, ONLY: bissec

    !Deactivate implicit variables
    IMPLICIT NONE

    real :: x,f,a,b

    !Declaring interfaces with subroutines
    INTERFACE
        SUBROUTINE polynomial(x,f)
            REAL(8),INTENT(IN) :: x
            REAL(8),INTENT(OUT) :: f
        END SUBROUTINE polynomial
    END INTERFACE

    CONTAINS

    x = (b-a)/2
    bissec(polynomial(x,f),[a,b])

END PROGRAM

subroutine polynomial(x,f)
    IMPLICIT NONE
    REAL,INTENT(IN) :: x
    REAL,INTENT(OUT) :: f

    !Create real polynomial function with known-zeros
    f = x**(3.) - 9*x + 3
END subroutine polynomial
