! PROGRAM CREATED TO INTRO TO SCIENTIFIC COMPUTATION COURSE OF IME`S POST-GRADUATION ENGINEERING COURSE
! WRITTEN BY WALLACE RAMOS ROSENDO DA SILVA
! PROFESSOR ACHILES ARANTES BASSI
! EXERCISE 2D FROM PAGE 206 - RUGGIERO & LOPES' BOOK
! COMPILING CODE: 
! Created a makefile to optimize compiling process
! Enter in Terminal and write:
! make ; ./ex2dpag206.exe argument1 ==> argument1 = number of repetitions for this exercise

! ANALYTICAL CORRECT ANSWER: x = transpose of (1 1) ROSEMBROCK
! ANALYTICAL CORRECT ANSWER: x = transpose of (1 1 1 1 1 1 1 1 1) BROYDEN

program main
    use fgsl
    use lapack_interfaces, only: dgetrf, dgetri
    use functions
    use jacobian
    use nonlinear

    implicit none

    ! Defining a name for Non-Linear Systems Method
    character(kind=fgsl_char,len=10) :: exercise
    integer(fgsl_int) :: iter=1, nmax, newunit, lwork
    real(fgsl_double) :: jdet
    real(fgsl_double), allocatable :: xvec(:), yvec(:), jmatrix(:,:),jmatrix_inv(:,:),newton_result(:)
        
    ! Calculating f(x), Jacobian and its inverse
    select case(exercise)
    case('rosembrock')
        nmax = 2
        allocate(xvec(nmax),yvec(nmax),jmatrix(nmax,nmax),jmatrix_inv(nmax,nmax),newton_result(nmax))
        xvec = (/-1.2_fgsl_double,1.0_fgsl_double/)
        yvec = rosembrock(xvec)
        jmatrix = jacobian_rosembrock(xvec)
    case('broyden')
        nmax = 10
        allocate(xvec(nmax),yvec(nmax),jmatrix(nmax,nmax),jmatrix_inv(nmax,nmax),newton_result(nmax))
        xvec = -1.0_fgsl_double
        yvec = tridiagonal_broyden(xvec)
        jmatrix = jacobian_tribroyden(xvec)
    case default
        print *, "Select a correct option between broyden or rosembrock. Try again."
    end select 
    
    lwork = 64*n
    ! Non-Linear Newton's Resolution Method
    call newton(nmax,xvec,yvec,jmatrix,lwork,newton_result)

    print '(f12.8)', newton_result

    deallocate(xvec,yvec,jmatrix,jmatrix_inv,newton_result)

end program main