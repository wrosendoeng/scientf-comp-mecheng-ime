module nonlinear
    use fgsl
    use functions
    use jacobian_matrices
    use direct_resolution_methods
    use iterative_resolution_methods
    implicit none
    contains
    
    subroutine newton(n, x, func, jacobiantype, resmethod, lwork, result)
        character(kind=fgsl_char,len=*) :: func, jacobiantype, resmethod
        integer(fgsl_int) :: k, n, ipiv(n), info, lwork, work(lwork)
        real(fgsl_double) :: e1 = 1.0e-18, e2 = 1.0e-18, s(n), x(n), f(n), j(n,n), result(n)
        
        s = 0.0
        do k = 1, 1000
            ! Select function and its jacobian
            if (func == "2d" .and. jacobiantype == "analytical") then
                f = rosembrock(x)
                j = jacobian_rosembrock(x)
            else if (func == "2d" .and. jacobiantype == "numerical") then
                f = rosembrock(x)
                j = general_jacobian(x,func,n)
            else if (func == "2e" .and. jacobiantype == "analytical") then
                f = tridiagonal_broyden(x,n)
                j = jacobian_tribroyden(x,n)
            else if (func == "2e" .and. jacobiantype == "numerical") then
                f = tridiagonal_broyden(x,n)
                j = general_jacobian(x,func,n)
            else
                print *, "At least one of arguments were written wrong. Try again."
		        stop
            end if
            
            if (maxval(abs(f)) < e1) then
                result = x
            else
                select case(resmethod)
                case('plu')
                    call lufactorization(j,-f,s,n)
                case('jacobi')
                    call gauss_jacobi(j,-f,s,n)
                case('seidel')
                    call gauss_seidel(j,-f,s,n)
		        case default
		            print *, "No one of resolution methods were chosen. Try again."
                end select
                result = x + s
                if (maxval(abs(s)) < e2) return
                x = x + s
            end if
        end do

        if (maxval(abs(f)) < e1 .and. maxval(abs(s)) < e2) print *, "Convergence has not reached in 100 iterations."

    end subroutine newton

    subroutine broyden(n, x, func, jacobiantype, resmethod, lwork, result)
        character(kind=fgsl_char,len=*) :: func, jacobiantype, resmethod
        integer(fgsl_int) :: k, n, ipiv(n), info, lwork, work(lwork)
        real(fgsl_double) :: e1 = 1.0e-18, e2 = 1.0e-18, s(n), x(n), f(n), j(n,n), result(n)
        
        s = 0.0
        do k = 1, 1000
            ! Select function and its jacobian
            if (func == "2d" .and. jacobiantype == "analytical") then
                f = rosembrock(x)
                j = jacobian_rosembrock(x)
            else if (func == "2d" .and. jacobiantype == "numerical") then
                f = rosembrock(x)
                j = general_jacobian(x,func,n)
            else if (func == "2e" .and. jacobiantype == "analytical") then
                f = tridiagonal_broyden(x,n)
                j = jacobian_tribroyden(x,n)
            else if (func == "2e" .and. jacobiantype == "numerical") then
                f = tridiagonal_broyden(x,n)
                j = general_jacobian(x,func,n)
            else
                print *, "At least one of arguments were written wrong. Try again."
		        stop
            end if
            
            if (maxval(abs(f)) < e1) then
                result = x
            else
                select case(resmethod)
                case('plu')
                    call lufactorization(j,-f,s,n)
                case('jacobi')
                    call gauss_jacobi(j,-f,s,n)
                case('seidel')
                    call gauss_seidel(j,-f,s,n)
		        case default
		            print *, "No one of resolution methods were chosen. Try again."
                end select
                result = x + s
                if (maxval(abs(s)) < e2) return
                x = x + s
            end if
        end do

        if (maxval(abs(f)) < e1 .and. maxval(abs(s)) < e2) print *, "Convergence has not reached in 100 iterations."

    end subroutine broyden

end module