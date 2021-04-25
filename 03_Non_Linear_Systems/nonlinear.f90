module nonlinear
    use fgsl
    use functions
    use lapack_interfaces, only: dgetrf, dgetri
    implicit none
    contains
    
    subroutine newton(n, x, f, j, lwork, result)
        integer(fgsl_int) :: k, n, ipiv(n), info, work(lwork)
        real(fgsl_double) :: e1 = 1.0e-6, e2 = 1.0e-6, s(n), x(n), f(n), j(n,n), &
        & j_inv(n,n), result(n)

        do k = 1, 100
            f = rosembrock(x)
            j = jacobian(x)
	    call dgetrf(n,n,j,n,ipiv,info)
            if (info == 0) then
            	call dgetri(n,j,n,ipiv,work,lwork,info)
            else
            	print *, "The factor U is singular."
            end if
            
            if (maxval(abs(f)) < e1) then
                result = x
            else
                s = -matmul(j_inv,f)
                result = x + s
                if (maxval(abs(s)) < e2) then
                    result = x + s
                    return
                else
                    x = x + s
                end if
            end if
        end do

        if (maxval(abs(f)) < e1 .and. maxval(abs(s)) < e2) print *, "Convergence has not reached in 100 iterations."

    end subroutine newton

    ! subroutine newtondiscrete(x, f, j_inv, result)
    !     implicit none
    !     integer(fgsl_int) :: k, n
    !     real(fgsl_double), allocatable :: s(:)
    !     real(fgsl_int),intent(in), allocatable :: x(:), f(:), j_inv(:,:)
    !     real(fgsl_double),intent(out) ::  result

    !     allocate(x(n))
    !     allocate(s(n))
    !     allocate(f(n))
    !     allocate(j_inv(n,n))

    !     do k = 1, n
    !         call rosembrock(x, f)
    !         if (maxval(abs(f)) < e1) then
    !             result = x
    !         else
    !             s = matmul(j_inv,-f)
    !             if (maxval(abs(s)) < e1) then
    !                 result = x + s
    !                 return
    !             else
    !                 x = x + s
    !             end if
    !         end if
    !     end do
    
    ! end subroutine newtondiscrete

end module
