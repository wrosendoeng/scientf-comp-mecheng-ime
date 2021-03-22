PROGRAM main

  !Declaring precompiled module named "numerical methods" and "func"
  USE methods
  USE func

  !Deactivate implicit variables
  IMPLICIT NONE
  REAL(8) :: a, b, x, bis

  !Assuming interval of a real function
  OPEN(10, FILE="data.txt", ACCESS='DIRECT', ACTION='READ', &
  STATUS='OLD',REC=exp)

  READ(10,'(F10.8)') a
  READ(10,'(F10.8)') b

  !a = 0
  !b = 1.5
  x = (b+a)/2 ! arithmetic mean of interval [a,b]
  bis = bissec(f,a,b)

END PROGRAM
