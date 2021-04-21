<<<<<<< HEAD
MODULE numerical_differentiation
    CONTAINS
    REAL(8) FUNCTION nf(f,x) ! Newton-forward formula of differentiation
        REAL(8) :: dx = 1.e-5, f, x
        !Create numerical method
        nf = (f(x+dx) - f(x))/dx
    END FUNCTION 
=======
MODULE numerical_differentiation
    CONTAINS
    REAL(8) FUNCTION nf(f,x) ! Newton-forward formula of differentiation
        REAL(8) :: dx = 1.e-5, f, x
        !Create numerical method
        nf = (f(x+dx) - f(x))/dx
    END FUNCTION 
>>>>>>> 2bd4f61f96afd92a8f1564a188da886181568064
END MODULE