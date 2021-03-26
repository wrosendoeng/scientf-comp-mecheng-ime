MODULE tested_function !Function to be tested
  IMPLICIT NONE  
  CONTAINS
    REAL(8) FUNCTION f(x)
      REAL(8),INTENT(IN) :: x
      !Create real polynomial function with known-zeros
      !f = 4.0*cos(x) - exp(x)
      f = x**(3) - 9*x + 3
    END function f
END MODULE tested_function
