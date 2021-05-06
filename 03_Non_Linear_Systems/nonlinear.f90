module nonlinear
    use fgsl
    use functions
    use jacobian_matrices
    use direct_resolution_methods
    use iterative_resolution_methods
    implicit none
    contains
    
    ! Newton and Discrete Newton uses the same subroutine, differing only
    ! by the jacobian, obtained analytically or numerically
    subroutine newton(n, x, func, jacobiantype, resmethod, result, iterations)
        character(kind=fgsl_char,len=*) :: func, jacobiantype, resmethod
        integer(fgsl_int) :: k, n, iterations
        real(fgsl_double) :: tol1 = 1.e-4, tol2 = 1.e-4, s(n), x(n), f(n), jac(n,n), result(n)
        
        s = 0.0
        do while (k <= 1000)
            iterations = k
            ! Select function and its jacobian
            if (func == '2d') then
                f = rosembrock(x)
                select case(jacobiantype)
                case('analytical')
                    jac = jacobian_rosembrock(x)
                case('numerical')
                    jac = general_jacobian(x,func,n)
                end select
            else if (func == '2e') then
                f = tridiagonal_broyden(x,n)
                select case(jacobiantype)
                case('analytical')
                    jac = jacobian_tribroyden(x,n)
                case('numerical')
                    jac = general_jacobian(x,func,n)
                end select
            else
                print *, "At least one of the arguments was written wrong. Try again."
		        stop
            end if
            if (maxval(abs(f)) <= tol1) then
                result = x
                return
            else
                select case(resmethod)
                case('plu')
                    call lufactorization(jac,-f,s,n)
                case('jacobi')
                    call gauss_jacobi(jac,-f,s,n)
                case('seidel')
                    call gauss_seidel(jac,-f,s,n)
                case default
                    print *, "No one of the resolution methods was chosen. Try again."
                end select
                if (maxval(abs(s)) <= tol2) then
                    result = x + s
                    return
                end if
            end if
            x = x + s
            k = k + 1
        end do

    end subroutine newton

    ! Quasi-Newton Brodyen's Method
    subroutine broyden(n, x, func, jacobiantype, resmethod, result, iterations)
        character(kind=fgsl_char,len=*) :: func, jacobiantype, resmethod
        integer(fgsl_int) :: i, j, k, n, iterations
        real(fgsl_double) :: tol1 = 1.0e-4, tol2 = 1.0e-4, s(n), x(n), f(n), newf(n), B(n,n), result(n), y(n), u(n)
        
        s = 0.0
        do while (k <= 1000)
            iterations = k        
            ! Select function and its jacobian
            if (func == "2d") then
                f = rosembrock(x)
                B = jacobian_rosembrock(x)
                select case(resmethod)
                case('plu')
                    call lufactorization(B,-f,s,n)
                case('jacobi')
                    call gauss_jacobi(B,-f,s,n)
                case('seidel')
                    call gauss_seidel(B,-f,s,n)
		        case default
		            print *, "No one of the resolution methods was chosen. Try again."
                end select
                newf = rosembrock(x+s)   
            else if (func == "2e") then
                f = tridiagonal_broyden(x,n)
                B = jacobian_tribroyden(x,n)
                select case(resmethod)
                case('plu')
                    call lufactorization(B,-f,s,n)
                case('jacobi')
                    call gauss_jacobi(B,-f,s,n)
                case('seidel')
                    call gauss_seidel(B,-f,s,n)
		        case default
		            print *, "No one of the resolution methods was chosen. Try again."
                end select
                newf = tridiagonal_broyden(x+s,n)
            else
                print *, "At least one of the arguments was written wrong. Try again."
		        stop
            end if
            if (maxval(abs(f)) < tol1) then
                result = x
                return
            end if
            y = (newf - f)
            u = (y - matmul(B,s))/matmul(reshape(s,(/1,n/)),s)
            B = B + matmul(reshape(u,(/n,1/)),reshape(s,(/1,n/)))  
            if (maxval(abs(s)) < tol2) then
                result = x + s
                return
            end if
            x = x + s
            k = k + 1
        end do

    end subroutine broyden

end module