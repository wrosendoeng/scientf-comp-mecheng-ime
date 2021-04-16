! MAIN PROGRAM - 2ND HOMEWORK
! Author: Wallace Ramos Rosendo da Silva
! Mechanical Engineering Post-Graduation - PGMEC SE/A
! Professor Dr. Mj. Arantes 
! Brazilian Army Military Institute of Engineering (IME)

PROGRAM linear_systems

    INTERFACE
        subroutine matrixA(A_matrix,n,lunit)
            INTEGER, INTENT(IN)    :: n, lunit
            REAL(4), INTENT(INOUT) :: A_matrix(n,n) 
        END subroutine matrixA
    
        subroutine colvectorB(B_colvector,n,lunit)
            INTEGER, INTENT(IN)    :: n, lunit
            REAL(4), INTENT(INOUT) :: B_colvector(n,1) 
        END subroutine colvectorB
    END INTERFACE

    use direct_resolution_methods

    integer(4) :: i, a_unit, n_rows = 17
    real(4), pointer, dimension(:,:) :: A_matrix
    real(4), pointer, dimension(:,1) :: B_colvector	
    ! integer(4) :: i, j, k, r

    OPEN(newunit = a_unit, action = 'read', status='old', file='matrixA.txt', &
    & access ='sequential', form = 'formatted', recl = 100)
    OPEN(newunit = b_unit, action = 'read', status='old', file='vectorB.txt', &
    & access ='sequential', form = 'formatted', recl = 100)
    
    allocate(A_matrix(n_rows,n_rows))
    allocate(B_colvector(n_rows,1))
    
    ! Reading Matrices A and B
    call matrixA(A_matrix,n_rows,a_unit)
    call matrixA(B_colvector,n_rows,b_unit)

    deallocate(A_matrix)
    deallocate(B_colvector)

    ! INTERFACE
    !     FUNCTION colvectorB(n)
    !         REAL, INTENT(IN) :: n
    !     END FUNCTION colvectorB
    ! END INTERFACE

END PROGRAM linear_systems

subroutine matrixA(A_matrix,n,lunit)
    implicit NONE
    integer(4) :: i
    integer(4), intent(in) :: n, lunit
    real(4), intent(inout) :: A_matrix(n,n)

    do i = 1, n
        read(lunit,*) A_matrix(i,:)
    end do

end subroutine matrixA

subroutine colvectorB(B_colvector,n,lunit)
    implicit NONE
    integer(4) :: i
    integer(4), intent(in) :: n, lunit
    real(4), intent(inout) :: B_colvector(n_rows,1)
    
    do i = 1, n
        read(lunit,*) B_colvector(i,:)
    end do

end subroutine colvectorB