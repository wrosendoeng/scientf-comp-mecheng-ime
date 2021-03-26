MODULE numerical_differentiation
CONTAINS
    REAL(8) function newton_forward(f,x) ! Newton-forward formula of differentiation
        REAL(8) :: dx
        REAL(8),INTENT(IN) :: x, f
        dx = 1.e-1
        !Create numerical method
        newton_forward = (f(x+dx) - f(x))/dx
    END function newton_forward
END MODULE
