PROGRAM main

  !Declaring precompiled module named "numerical methods" and "func"
  USE methods
  USE func

  !Deactivate implicit variables
  IMPLICIT NONE
  REAL(8) :: a, b, x, bis

  !Assuming interval of a real function
  a = 0
  b = 1
  x = (b+a)/2 ! arithmetic mean of interval [a,b]
  bis = bissec(f,a,b)

END PROGRAM
