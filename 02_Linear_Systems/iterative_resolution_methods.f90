<<<<<<< HEAD
! GAUSS-SEIDEL & GAUSS-JACOBI 
! ITERATIVE NUMERICAL METHODS

MODULE iterative_resolution_methods
    
    implicit none 
    contains

    subroutine gauss_seidel(a,b,x,n) ! Gauss-Seidel Method

        logical :: linecriteria(n)
        integer(4) :: i, j, k=0, n, itermax=100, l_pivot
        real(8) :: pivot, troca, soma, sum1, sum2, tol = 1.0e-7
        real(8) :: a(n,n), b(n), maximum(n), x0(n)
        real(8), intent(out) :: x(n)
        
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

        x0 = 2.0e1
        do while (k <= itermax .and. any(linecriteria) .eqv. .true.)
            do i = 1, n
                x(i) = 0.0e0           
                do j = 1, n 
                    if (i /= j) then
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
        end if

    end subroutine gauss_seidel

    subroutine gauss_jacobi(a,b,x,n)
        
        logical :: linecriteria(n)
        integer(4) :: i, j, k=0, n, itermax=100, l_pivot
        real(8) :: pivot, troca, soma, tol = 1.0e-7
        real(8) :: a(n,n), b(n), c(n,n), id(n,n), did(n,n), y(n), maximum(n), x0(n)
        real(8), intent(out) :: x(n)

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

        do i = 1, n 
            id(i,i) = 1d0
            did(i,i) = 1d0/a(i,i)
        end do

        x0 = 2.0e1
        c = id + matmul(did,a)
        y = matmul(did,b)

        do while (k <= itermax .and. any(linecriteria) .eqv. .true.)
            x = y + matmul(c,x0)
            if (maxval(abs(x-x0)) < tol) then
                return
            end if
            x0 = x
            k = k + 1
        end do

        if (any(linecriteria) .eqv. .false.) then
            print *, "The system does not converge."
        end if

    end subroutine gauss_jacobi

=======
! GAUSS-SEIDEL & GAUSS-JACOBI 
! ITERATIVE NUMERICAL METHODS

MODULE iterative_resolution_methods
    
    implicit none 
    contains

    subroutine gauss_seidel(a,b,x,n) ! Gauss-Seidel Method

        logical :: linecriteria(n)
        integer(4) :: i, j, k=0, n, itermax=100, l_pivot
        real(8) :: pivot, troca, soma, sum1, sum2, tol = 1.0e-7
        real(8) :: a(n,n), b(n), maximum(n), x0(n)
        real(8), intent(out) :: x(n)
        
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

        x0 = 2.0e1
        do while (k <= itermax .and. any(linecriteria) .eqv. .true.)
            do i = 1, n
                x(i) = 0.0e0           
                do j = 1, n 
                    if (i /= j) then
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
        end if

    end subroutine gauss_seidel

    subroutine gauss_jacobi(a,b,x,n)
        
        logical :: linecriteria(n)
        integer(4) :: i, j, k=0, n, itermax=100, l_pivot
        real(8) :: pivot, troca, soma, tol = 1.0e-7
        real(8) :: a(n,n), b(n), c(n,n), id(n,n), did(n,n), y(n), maximum(n), x0(n)
        real(8), intent(out) :: x(n)

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

        do i = 1, n 
            id(i,i) = 1d0
            did(i,i) = 1d0/a(i,i)
        end do

        x0 = 2.0e1
        c = id + matmul(did,a)
        y = matmul(did,b)

        do while (k <= itermax .and. any(linecriteria) .eqv. .true.)
            x = y + matmul(c,x0)
            if (maxval(abs(x-x0)) < tol) then
                return
            end if
            x0 = x
            k = k + 1
        end do

        if (any(linecriteria) .eqv. .false.) then
            print *, "The system does not converge."
        end if

    end subroutine gauss_jacobi

>>>>>>> 2bd4f61f96afd92a8f1564a188da886181568064
END MODULE iterative_resolution_methods