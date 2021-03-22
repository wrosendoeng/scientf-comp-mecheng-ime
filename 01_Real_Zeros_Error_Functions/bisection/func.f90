MODULE func
  implicit none
  CONTAINS
    REAL(8) FUNCTION f(x)
      REAL(8),INTENT(IN) :: x

      !Create real polynomial function with known-zeros
      f = x**(3.) - 9*x + 3
    END function f
END MODULE func