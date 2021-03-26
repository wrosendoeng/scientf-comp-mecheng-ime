PROGRAM main

  !Declaring precompiled module named "numerical methods" and "func"
  USE methods
  USE func
  !USE getopt_m

  !Deactivate implicit variables
  IMPLICIT NONE
  CHARACTER(len=32):: value_arg
  REAL(8) :: a, b, x, bis, t
  REAL(8), DIMENSION(2) :: vec
  INTEGER :: i

  !Assuming interval of a real function using a .txt file
  OPEN(10, FILE="data.txt",ACCESS='SEQUENTIAL', ACTION='READ', &
  STATUS='OLD',FORM='FORMATTED')

  READ(10,'(F10.8)') a
  READ(10,'(F10.8)') b

  !Passing command-line arguments
  !i = 0
  !DO 
    !CALL GET_COMMAND_ARGUMENT(i,value_arg)
    !IF (LEN_TRIM(value_arg) == 0) EXIT
	
    !WRITE(10,*) TRIM(value_arg)
    !READ(10,*) t
    !vec(i) = t	
    !i = i+1
  !END DO

  a = vec(1)
  b = vec(2)
  
  x = (b+a)/2 ! arithmetic mean of interval [a,b]
  bis = bissec(f,a,b)

END PROGRAM
