MODULE numerical_differentiation
    CONTAINS
    REAL(8) FUNCTION nf(f,x) ! Newton-forward formula of differentiation
        REAL(8) :: dx = 1.e-5, f, x
        !Create numerical method
        nf = (f(x+dx) - f(x))/dx
    END FUNCTION 
END MODULE