! GAUSS-SEIDEL & GAUSS-JACOBI ITERATIVE NUMERICAL METHODS

MODULE iterative_resolution_methods
    use fgsl
    implicit none 
    contains

    subroutine gauss_seidel(a,b,x,n) ! Gauss-Seidel Method

        logical :: linecriteria(n), sascriteria(n), diagcriteria(n)
        integer(fgsl_int) :: i, j, k, n, itermax=100, l_pivot
        real(fgsl_double) :: pivot, troca, tol = 1.0e-18
        real(fgsl_double) :: a(n,n), b(n), soma, soma1, soma2, maximum(n), x0(n), beta(n), domdiag(n)
        real(fgsl_double), intent(out) :: x(n)
        
        !Pivoting    
        do k = 1, n-1
            pivot = a(k,k)
            l_pivot = k 
            do i = (k+1),n
                if (abs(a(i,k)) > abs(pivot)) then 
                    pivot = a(i,k)
                    l_pivot = i 
                end if
            end do
            if (pivot == 0.0) exit
            if (l_pivot /= k) then
                troca = b(k)
                b(k) = b(l_pivot)
                b(l_pivot) = troca
                do j = 1, n
                    troca = a(k,j)
                    a(k,j) = a(l_pivot,j)
                    a(l_pivot,j) = troca
                end do
            end if
        end do

        ! Dominant diagonal
        do i = 1, n 
            do j = 1, n
                if (i /= j) then
                    domdiag(i) = domdiag(i) + abs(a(i,j)/a(i,i))
                end if  
            end do
            if (domdiag(i) < 1.0_fgsl_double) then
                diagcriteria(i) = .true.
            else
                diagcriteria(i) = .false.
            end if
        end do

        ! Sassenfeld Criteria
        do i = 1, n
            do j = 1, i-1
                soma1 = soma1 + abs(a(i,j))*beta(j)
            end do
            do j = i+1, n
                soma2 = soma2 + abs(a(i,j)) 
            end do
            soma = (soma1+soma2)/abs(a(i,i))
            if (beta(i) < soma) then
                beta(i) = soma
            end if
            if (beta(i) < 1.0_fgsl_double) then
                sascriteria(i) = .true.
            else
                sascriteria(i) = .false.
            end if
        end do

        ! Criteria of Lines
        maximum = 0.0    
        do i = 1, n
            soma = 0.0
            do j = 1, n
                if (i /= j) then
                    soma = soma + abs(a(i,j))
                end if
            end do
            soma = soma/abs(a(i,i))
            if (maximum(i) < soma) then
                maximum(i) = soma
            end if
        end do

        if (maxval(maximum) < 1.0_fgsl_double) then
            linecriteria = .true.
        else
            linecriteria = .false.
        end if
        
        do while (k <= itermax .and. any(sascriteria) .eqv. .true.)
            do i = 1, n         
                do j = 1, i-1
                    x(i) = x(i) + a(i,j)*x(j)
                end do
                do j = i+1, n
                    x(i) = x(i) + a(i,j)*x0(j)
                end do
                x(i) = (b(i)-x(i))/a(i,i)
            end do
            if (maxval(abs(x-x0)) < tol) then
                return
            end if
            x0 = x
            k = k + 1
        end do

        if (any(sascriteria) .eqv. .false.) then
            print *, "The system does not converge."
            stop
        end if

    end subroutine gauss_seidel

    subroutine gauss_jacobi(a,b,x,n)
        
        logical :: linecriteria(n)
        integer(fgsl_int) :: i, j, k, n, itermax=100, l_pivot
        real(fgsl_double) :: pivot, troca, soma, tol = 1.0e-18
        real(fgsl_double) :: a(n,n), b(n), maximum(n), x0(n)
        real(fgsl_double), intent(out) :: x(n)

        !Pivoting    
        do k = 1, n-1
            pivot = a(k,k)
            l_pivot = k 
            do i = (k+1),n
                if (abs(a(i,k)) > abs(pivot)) then 
                    pivot = a(i,k)
                    l_pivot = i 
                end if
            end do
            if (pivot == 0.0) exit
            if (l_pivot /= k) then
                troca = b(k)
                b(k) = b(l_pivot)
                b(l_pivot) = troca
                do j = 1, n
                    troca = a(k,j)
                    a(k,j) = a(l_pivot,j)
                    a(l_pivot,j) = troca
                end do
            end if
        end do

        ! Criteria of Lines
        maximum = 0.0    
        do i = 1, n
            soma = 0.0
            do j = 1, n
                if (i /= j) then
                    soma = soma + abs(a(i,j))
                end if
            end do
            soma = soma/abs(a(i,i))
            if (maximum(i) < soma) then
                maximum(i) = soma
            end if
            if (maximum(i) < 1.0) then
                linecriteria(i) = .true.
            else
                linecriteria(i) = .false.
            end if
        end do

        x0 = 0.0e0
	    k = 1
        do while (k <= itermax .and. any(linecriteria) .eqv. .true.)
            do i = 1, n           
                do j = 1, n 
                    if (j /= i) then
                        x(i) = x(i) + a(i,j)*x0(j)
                    end if
                end do
                x(i) = (b(i)-x(i))/a(i,i)
            end do
            if (maxval(abs(x-x0)) < tol) then
                return
            end if
	        x0 = x
            k = k + 1
        end do

        if (any(linecriteria) .eqv. .false.) then
            print *, "The system does not converge."
            stop
        end if

    end subroutine gauss_jacobi

END MODULE iterative_resolution_methods
