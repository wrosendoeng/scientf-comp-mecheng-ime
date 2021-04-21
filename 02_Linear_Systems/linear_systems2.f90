<<<<<<< HEAD
! MAIN PROGRAM - 2ND HOMEWORK
! Author: Wallace Ramos Rosendo da Silva
! Mechanical Engineering Post-Graduation - PGMEC SE/A
! Professor Dr. Mj. Arantes 
! Brazilian Army Military Institute of Engineering (IME)

PROGRAM linear_systems

    use direct_resolution_methods, only: gausselimination !lu_factorization
    use iterative_resolution_methods, only: gauss_seidel ! or gauss_jacobi

    INTERFACE
        subroutine matrixA(coefmatrix,numrows,matrixunit)
            INTEGER(4) :: rows, numrows, matrixunit
            REAL(8) :: coefmatrix(numrows,numrows) 
        END subroutine matrixA
    
        subroutine colvectorB(columnvector,numrows,vecunit)
            INTEGER(4) :: rows, numrows, vecunit
            REAL(8) :: columnvector(numrows) 
        END subroutine colvectorB
    END INTERFACE

    integer(4) :: a_unit, a2_unit, b_unit, b2_unit, rc, x_unit, xrows, xcols, n_rows = 17
    real(8), allocatable :: A_matrix(:,:)
    real(8), allocatable :: B_colvector(:)
    real(8), allocatable :: vectorX(:)

    allocate(A_matrix(n_rows,n_rows))
    allocate(B_colvector(n_rows))
    allocate(vectorX(n_rows))

    ! Reading Matrices A and B
    call matrixA(A_matrix,nmax,a_unit)
    call colvectorB(B_colvector,n_rows,b_unit)

    ! Choosing method to calculate:
    call gausselimination(A_matrix,B_colvector,n_rows)
    ! call lu_factorization(A_matrix,B_colvector,n_rows)
    ! call gauss_seidel()
    ! call gauss_jacobi()   
    
    do xrows = 1, n_rows
        write(x_unit,*) (vectorX(xrows))
        write(a2_unit,*) (A_matrix(xrows,xcols),xcols=1,n_rows)
        write(b2_unit,*) (B_colvector(xrows))
    end do 

    deallocate(A_matrix)
    deallocate(B_colvector)
    deallocate(vectorX)

END PROGRAM linear_systems

subroutine matrixA(coefmatrix,nmax,matrixunit)
    implicit NONE
    integer(4) :: rows, cols, nmax, matrixunit, ios
    REAL(8), DIMENSION(nmax,nmax) :: coefmatrix
    
    do rows = 1, nmax
        write(matrixunit,*,iostat=ios) (coefmatrix(rows,cols),cols=1,nmax)
    end do


end subroutine matrixA

subroutine colvectorB(columnvector,nmax,vecunit)
    implicit NONE
    integer(4) :: rows, nmax, vecunit, ios  
    real(8) :: columnvector(nmax)
    
    read(vecunit,*,iostat=ios) (columnvector(rows),rows=1,nmax)

=======
! MAIN PROGRAM - 2ND HOMEWORK
! Author: Wallace Ramos Rosendo da Silva
! Mechanical Engineering Post-Graduation - PGMEC SE/A
! Professor Dr. Mj. Arantes 
! Brazilian Army Military Institute of Engineering (IME)

PROGRAM linear_systems

    use direct_resolution_methods, only: gausselimination !lu_factorization
    use iterative_resolution_methods, only: gauss_seidel ! or gauss_jacobi

    INTERFACE
        subroutine matrixA(coefmatrix,numrows,matrixunit)
            INTEGER(4) :: rows, numrows, matrixunit
            REAL(8) :: coefmatrix(numrows,numrows) 
        END subroutine matrixA
    
        subroutine colvectorB(columnvector,numrows,vecunit)
            INTEGER(4) :: rows, numrows, vecunit
            REAL(8) :: columnvector(numrows) 
        END subroutine colvectorB
    END INTERFACE

    integer(4) :: a_unit, a2_unit, b_unit, b2_unit, rc, x_unit, xrows, xcols, n_rows = 17
    real(8), allocatable :: A_matrix(:,:)
    real(8), allocatable :: B_colvector(:)
    real(8), allocatable :: vectorX(:)

    allocate(A_matrix(n_rows,n_rows))
    allocate(B_colvector(n_rows))
    allocate(vectorX(n_rows))

    ! Reading Matrices A and B
    call matrixA(A_matrix,nmax,a_unit)
    call colvectorB(B_colvector,n_rows,b_unit)

    ! Choosing method to calculate:
    call gausselimination(A_matrix,B_colvector,n_rows)
    ! call lu_factorization(A_matrix,B_colvector,n_rows)
    ! call gauss_seidel()
    ! call gauss_jacobi()   
    
    do xrows = 1, n_rows
        write(x_unit,*) (vectorX(xrows))
        write(a2_unit,*) (A_matrix(xrows,xcols),xcols=1,n_rows)
        write(b2_unit,*) (B_colvector(xrows))
    end do 

    deallocate(A_matrix)
    deallocate(B_colvector)
    deallocate(vectorX)

END PROGRAM linear_systems

subroutine matrixA(coefmatrix,nmax,matrixunit)
    implicit NONE
    integer(4) :: rows, cols, nmax, matrixunit, ios
    REAL(8), DIMENSION(nmax,nmax) :: coefmatrix
    
    do rows = 1, nmax
        write(matrixunit,*,iostat=ios) (coefmatrix(rows,cols),cols=1,nmax)
    end do


end subroutine matrixA

subroutine colvectorB(columnvector,nmax,vecunit)
    implicit NONE
    integer(4) :: rows, nmax, vecunit, ios  
    real(8) :: columnvector(nmax)
    
    read(vecunit,*,iostat=ios) (columnvector(rows),rows=1,nmax)

>>>>>>> 2bd4f61f96afd92a8f1564a188da886181568064
end subroutine colvectorB