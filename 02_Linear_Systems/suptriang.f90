<<<<<<< HEAD
!RESOLUTION OF A SUPERIOR TRIANGULAR SYSTEM
!Given a sup. triangular n x n with non-zeros elements in main matrix diagonal x_n, x_n-1,...,x_1 

MODULE parcial_pivot

    implicit none

    subroutine parcial(n_matrix, pivot_matrix, pv_value, aux, i, j, k, n, r) 

        integer(4) :: i, j, k, n, r  
        ! where i, j, k are iterators; n is 2d-array dimension of n_matrix &
        ! r is the dimension of vector pivot_matrix
        real(4) :: n_matrix(n,n), pivot_matrix(k), pv_value, aux, m_coef
        
        ! Creation of permatutation matrix P
        do i = 1, n 
            pivot_matrix(i) = i  
        end do

        do k = 1, n-1
            pv_value = abs(n_matrix(k,k))
            r = k 
            do i = (k+1), n
                if (abs(n_matrix(i,k)) < pv_value) then
                    pv_value = abs(n_matrix(i,k))
                    r = i
                end if
            end do
            if (pv_value == 0) then 
                STOP ! matrix A is singular
            end if  
            if (r /= k) then
                aux = pivot_matrix(k)
                pivot_matrix(k) = pivot_matrix(r)
                pivot_matrix(r) = aux
                do j = 1, n 
                    aux = n_matrix(k,j)
                    n_matrix(k,j) = n_matrix(r,j)
                    n_matrix(r,j) = aux
                end do
            end if
            
            do i = (k+1), n 
                m_coef = n_matrix(i,k)/n_matrix(k,k)
                n_matrix(i,k) = m_coef
                do j = (k+1),n 
                    n_matrix(i,j) = n_matrix(i,j) - m_coef*n_matrix(k,j)
                end do
            end do
        end do

    end subroutine parcial

=======
!RESOLUTION OF A SUPERIOR TRIANGULAR SYSTEM
!Given a sup. triangular n x n with non-zeros elements in main matrix diagonal x_n, x_n-1,...,x_1 

MODULE parcial_pivot

    implicit none

    subroutine parcial(n_matrix, pivot_matrix, pv_value, aux, i, j, k, n, r) 

        integer(4) :: i, j, k, n, r  
        ! where i, j, k are iterators; n is 2d-array dimension of n_matrix &
        ! r is the dimension of vector pivot_matrix
        real(4) :: n_matrix(n,n), pivot_matrix(k), pv_value, aux, m_coef
        
        ! Creation of permatutation matrix P
        do i = 1, n 
            pivot_matrix(i) = i  
        end do

        do k = 1, n-1
            pv_value = abs(n_matrix(k,k))
            r = k 
            do i = (k+1), n
                if (abs(n_matrix(i,k)) < pv_value) then
                    pv_value = abs(n_matrix(i,k))
                    r = i
                end if
            end do
            if (pv_value == 0) then 
                STOP ! matrix A is singular
            end if  
            if (r /= k) then
                aux = pivot_matrix(k)
                pivot_matrix(k) = pivot_matrix(r)
                pivot_matrix(r) = aux
                do j = 1, n 
                    aux = n_matrix(k,j)
                    n_matrix(k,j) = n_matrix(r,j)
                    n_matrix(r,j) = aux
                end do
            end if
            
            do i = (k+1), n 
                m_coef = n_matrix(i,k)/n_matrix(k,k)
                n_matrix(i,k) = m_coef
                do j = (k+1),n 
                    n_matrix(i,j) = n_matrix(i,j) - m_coef*n_matrix(k,j)
                end do
            end do
        end do

    end subroutine parcial

>>>>>>> 2bd4f61f96afd92a8f1564a188da886181568064
END MODULE parcial_pivot