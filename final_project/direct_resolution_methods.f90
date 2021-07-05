MODULE direct_resolution_methods
    use parameters, only: wp => PARM_DP, intlength => PARM_SI, fnl => PARM_SCL
    implicit none
    contains 
    
    subroutine elimination(a,b,x,n)
        
        integer(intlength) :: i, j, k, l_pivot, n
        real(wp) :: m, pivot,soma, troca, a(n,n), b(n), x(n)
    
        ! Pivoting
        do k = 1, n-1
            pivot = a(k,k)
            l_pivot = k 
            do i = (k+1),n
                if (abs(a(i,k)) >= abs(pivot)) then 
                    pivot = a(i,k)
                    l_pivot = i 
                end if
            end do
            if (pivot == 0.0) exit
            if (l_pivot /= k) then
                do j = 1, n
                    troca = a(k,j)
                    a(k,j) = a(l_pivot,j)
                    a(l_pivot,j) = troca
                end do
                troca = b(k)
                b(k) = b(l_pivot)
                b(l_pivot) = troca
            end if

            ! Elimination
            do i = (k+1),n
                m = a(i,k)/a(k,k)
                a(i,k) = 0.0
                do j = (k+1),n
                    a(i,j) = a(i,j) - m*a(k,j)
                end do
                b(i) = b(i) - m*b(k)
            end do 
        end do
    
        ! Calculating upper-triangular system
        x(n) = b(n)/a(n,n)
        do i = (n-1),1,-1
            soma = 0.0
            do j = (i+1),n
                soma = soma + a(i,j)*x(j)
            end do
            x(i) = (b(i)-soma)/a(i,i)
        end do
        
    end subroutine elimination
    
    subroutine lufactorization(a,b,x,n)

        integer(intlength) :: aux, i, j, k, l_pivot, n, p(n)
        real(wp) :: m, pivot, troca, a(n,n), b(n), c(n), y(n)
        real(wp), intent(out) :: x(n)
    
        !Pivoting
        do i = 1, n 
            p(i) = i 
        end do
    
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
            do i = (k+1),n
                m = a(i,k)/a(k,k)
                a(i,k) = 0.0
                do j = (k+1),n
                    a(i,j) = a(i,j) - m*a(k,j)
                end do
                b(i) = b(i) - m*b(k)
            end do 
        end do
    
        ! c = Pb
        do i = 1, n 
            aux = p(i)
            c(i) = b(aux)
        end do
    
        ! Ly = Pb
        y = 0.0_wp
        do i = 1, n
            do j = 1, (n-1)
                y(i) = y(i) + a(i,j)*y(j)
            end do 
            y(i) = c(i) - y(i)
        end do
    
        ! Ux = y
        x = 0.0_wp
        do i = n,1,-1   
            do j = (i+1),n
                x(i) = x(i) + a(i,j)*x(j)
            end do
            x(i) = (y(i)-x(i))/a(i,i)
        end do

    end subroutine lufactorization

END MODULE direct_resolution_methods