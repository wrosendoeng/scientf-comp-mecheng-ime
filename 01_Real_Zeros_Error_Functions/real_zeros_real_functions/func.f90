MODULE func
  CONTAINS
    real function f(x)
      IMPLICIT NONE
      REAL(8),INTENT(IN) :: x
      REAL(8) :: f

      !Create real polynomial function with known-zeros
      f = x**(3.) - 9*x + 3
    END function f
END MODULE func
