PROGRAM main

    !Declaring precompiled module named "numerical methods"
    use numerical_methods
    use func

    !Deactivate implicit variables
    IMPLICIT NONE

    real :: x,f,a,b

    !Declaring interfaces with subroutines
    x = (b-a)/2
    CALL f(x)

    bissec(polynomial(x),[a,b])

END PROGRAM
