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
    subroutine newton(n, x, func, jacobiantype, resmethod, lwork, result, iterations)
        character(kind=fgsl_char,len=*) :: func, jacobiantype, resmethod
        integer(fgsl_int) :: k=1, n, ipiv(n), info, lwork, work(lwork), iterations
        real(fgsl_double) :: e1 = 1.0e-6, e2 = 1.0e-6, s(n), x(n), f(n), j(n,n), result(n)
        
        s = 0.0
        do while (k <= 1000)
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
                return
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
                x = x + s
            end if
            iterations = k
            k = k + 1
        end do

        if (k == 1000 .and. maxval(abs(f)) < e1 .and. maxval(abs(s)) < e2) then
            print ('(a,i4,a)'), "Convergence has not reached in ",k," iterations."
        end if

    end subroutine newton

    ! Quasi-Newton Brodyen's Method
    subroutine broyden(n, x, func, jacobiantype, resmethod, lwork, result, iterations)
        character(kind=fgsl_char,len=*) :: func, jacobiantype, resmethod
        integer(fgsl_int) :: i, j, k=1, n, ipiv(n), info, lwork, work(lwork), iterations, h = 1.e-3
        real(fgsl_double) :: e1 = 1.0e-6, e2 = 1.0e-6, s(n), x(n), f(n), newf(n), B(n,n), result(n), y(n), u(n)
        
        s = 0.0
        do while (k <= 1000 .or. maxval(abs(f)) > e1 .or. maxval(abs(s)) > e2)
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
		            print *, "No one of resolution methods were chosen. Try again."
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
		            print *, "No one of resolution methods were chosen. Try again."
                end select
                newf = tridiagonal_broyden(x+s,n)
            else
                print *, "At least one of arguments were written wrong. Try again."
		        stop
            end if
            y = (newf - f)
            u = (y - matmul(B,s))/matmul(reshape(s,(/1,n/)),s)
            B = B + matmul(reshape(u,(/n,1/)),reshape(s,(/1,n/)))  
            result = x + s
            x = result
            k = k + 1
        end do

    end subroutine broyden

end module