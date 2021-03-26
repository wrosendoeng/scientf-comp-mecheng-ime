MODULE func
  IMPLICIT NONE
  
  CONTAINS
    REAL(8) FUNCTION f(x)
      REAL(8),INTENT(IN) :: x
      !Create real polynomial function with known-zeros
      f = 4.0*cos(x) - exp(x)
      !f = x.**(3) - 9*x + 3
    END function  f

    REAL(8) function df(x)
      real(8),INTENT(IN) :: x, f 
      !Create 
      df = 
      !
    END function df

END MODULE func
