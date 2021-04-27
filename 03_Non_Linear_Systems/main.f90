! PROGRAM CREATED TO INTRO TO SCIENTIFIC COMPUTATION COURSE OF IME`S POST-GRADUATION ENGINEERING COURSE
! WRITTEN BY WALLACE RAMOS ROSENDO DA SILVA
! PROFESSOR ACHILES ARANTES BASSI
! EXERCISE 2D FROM PAGE 206 - RUGGIERO & LOPES' BOOK
! COMPILING CODE: 
! Created a makefile to optimize compiling process
! Enter in Terminal and write:
! make ; ./chap04nonlinear.exe argument1 ==> argument1 = number of repetitions for this exercise

! ANALYTICAL CORRECT ANSWER: x = transpose of (1 1) ROSEMBROCK
! ANALYTICAL CORRECT ANSWER: x = transpose of (1 1 1 1 1 1 1 1 1) BROYDEN

program main
    use fgsl
    use functions
    use jacobian_matrices
    use nonlinear
    use direct_resolution_methods
    use iterative_resolution_methods

    implicit none

    ! Defining a name for Non-Linear Systems Method
    character(kind=fgsl_char,len=10) :: exercise, jacobianoption, linearmethod
    integer(fgsl_int) :: iter=1, nmax, unit1, length_work
    real(fgsl_double), allocatable :: xvec(:),newton_result(:)
    
    call get_command_argument(1,exercise) !rosembrock = 2d && broyden = 2e
    call get_command_argument(2,jacobianoption) ! analytical (Newton) or numerical (Discrete Newton)
    call get_command_argument(3,linearmethod) ! (plu, jacobi or seidel)

    open(newunit=unit1,action='write',status='replace',access='sequential', &
    & file='final'//trim(exercise)//'_'//trim(jacobianoption)//'_'//trim(linearmethod)//'.txt')

    ! Calculating f(x), Jacobian and its inverse
    select case(exercise)
    case('2d')
        nmax = 2
        allocate(xvec(nmax),newton_result(nmax))
        xvec = (/-1.2_fgsl_double,1.0_fgsl_double/)
    case('2e')
        nmax = 10
        allocate(xvec(nmax),newton_result(nmax))
        xvec = -1.0_fgsl_double
    case default
        print *, "Select a correct option between broyden or rosembrock. Try again."
	stop
    end select 
    
    length_work = 64*nmax
    ! Non-Linear Newton's Resolution Method
    call newton(nmax, xvec, exercise, jacobianoption, linearmethod, length_work, newton_result)

    write(unit1,'(f18.14)') newton_result

    deallocate(xvec,newton_result)

end program main
