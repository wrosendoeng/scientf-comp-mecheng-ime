PROGRAM main

  !Declaring precompiled module named "numerical methods" and "func"
  USE numerical_methods, ONLY: newton_raphson
  USE numerical_differentiation, ONLY: newton_forward

  !Deactivate implicit variables
  IMPLICIT NONE
  REAL(8) :: a, b, met

  !Assuming interval of a real function using a .txt file
  OPEN(10, FILE="data.txt",ACCESS='SEQUENTIAL', ACTION='READ', &
  STATUS='OLD',FORM='FORMATTED')

  READ(10,'(F10.8)') a
  READ(10,'(F10.8)') b

  INTERFACE
    FUNCTION f(x)
      REAL, f 
      REAL, INTENT(IN) :: x
    END function f
  END INTERFACE
  
  met = newton_raphson(f,a,b)

END PROGRAM

CONTAINS

REAL(8) FUNCTION f(x)
  REAL(8) :: f
  REAL(8),INTENT(IN) :: x
  !Create real polynomial function with known-zeros
  !f = 4.0*cos(x) - exp(x)
  f = x.**(3) - 9*x + 3
END function  f