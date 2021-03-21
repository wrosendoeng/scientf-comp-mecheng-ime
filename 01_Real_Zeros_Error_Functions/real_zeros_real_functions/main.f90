PROGRAM main

    !Declaring precompiled module named "numerical methods"
    use numerical_methods

    !Deactivate implicit variables
    IMPLICIT NONE

    real :: x,f,a,b

    !Declaring interfaces with subroutines
    INTERFACE
        SUBROUTINE polynomial
            REAL(8),INTENT(IN) :: x
        END SUBROUTINE polynomial
    END INTERFACE

    CONTAINS

    x = (b-a)/2
    bissec(polynomial(x,f),[a,b])

END PROGRAM

REAL function polynomial(x)
    IMPLICIT NONE
    REAL,INTENT(IN) :: x

    !Create real polynomial function with known-zeros
    f = x**(3.) - 9*x + 3
END function polynomial
