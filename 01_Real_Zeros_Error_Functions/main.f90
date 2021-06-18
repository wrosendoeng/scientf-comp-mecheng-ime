PROGRAM main

  !Declaring precompiled module named "numerical methods", "numerical_differentiation" and "tested_functions"
  USE numerical_methods, ONLY : falsepos !bissec falsepos newton_raphson secant  
  !USE numerical_differentiation
  USE tested_function

  !Deactivate implicit variables
  IMPLICIT NONE
  REAL(8) :: x0, x1, met

  !Assuming interval of a real function using a .txt file
  OPEN(10, FILE="data.txt",ACCESS='SEQUENTIAL', ACTION='READ', &
  STATUS='OLD',FORM='FORMATTED')

  READ(10,'(F10.8)') x0
  READ(10,'(F10.8)') x1
  
  !met = bissec(f,x0,x1)
  met = falsepos(f,x0,x1)
  !met = newton_raphson(f,nf,x0,x1)
  !met = secant(f,x0,x1)

PROGRAM main

  !Declaring precompiled module named "numerical methods", "numerical_differentiation" and "tested_functions"
  USE numerical_methods, ONLY : falsepos !bissec falsepos newton_raphson secant  
  !USE numerical_differentiation
  USE tested_function

  !Deactivate implicit variables
  IMPLICIT NONE
  REAL(8) :: x0, x1, met

  !Assuming interval of a real function using a .txt file
  OPEN(10, FILE="data.txt",ACCESS='SEQUENTIAL', ACTION='READ', &
  STATUS='OLD',FORM='FORMATTED')

  READ(10,'(F10.8)') x0
  READ(10,'(F10.8)') x1
  
  !met = bissec(f,x0,x1)
  met = falsepos(f,x0,x1)
  !met = newton_raphson(f,nf,x0,x1)
  !met = secant(f,x0,x1)
  
END PROGRAM