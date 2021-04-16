MODULE partial_pivot
    
    implicit NONE
    integer(4) :: i, j, k

    contains 

    subroutine scaled_partial(mainmatrixA,vectorB,numrows)
        
        INTEGER(4) :: numrows, rowpivot
        REAL(8) :: auxiliar, sclpivot, mainmatrixA(numrows,numrows), multi_factor, swap(numrows), &
        & vectorB(numrows), vectorX(numrows)

        ! Scaled Partial Pivoting
        do i = 1, numrows
            swap(i) = i
        end do 

        do k = 1, numrows-1
            sclpivot = abs(mainmatrixA(k,k))
            rowpivot = k
            do i = k+1, numrows
                if (abs(mainmatrixA(i,k)) .gt. sclpivot) then 
                    sclpivot = abs(mainmatrixA(i,k))
                    rowpivot = i    
                end if 
            end do
            
            if (sclpivot .eq. 0) exit ! Matrix A is singular
            if (rowpivot /= k) then
                auxiliar = swap(k)
                swap(k) = swap(rowpivot)
                swap(rowpivot) = auxiliar
                do j = 1, numrows
                    auxiliar = mainmatrixA(k,j)
                    mainmatrixA(k,j) = mainmatrixA(rowpivot,j)
                    mainmatrixA(rowpivot,j) = auxiliar
                end do
            end if

            do i = k + 1, numrows
                multi_factor = mainmatrixA(i,k)/mainmatrixA(k,k)
                mainmatrixA(i,k) = multi_factor
                do j = k + 1, numrows
                    mainmatrixA(i,j) = mainmatrixA(i,j) - multi_factor*mainmatrixA(k,j)
                end do
            end do
        end do 
    
    end subroutine scaled_partial

end module partial_pivot