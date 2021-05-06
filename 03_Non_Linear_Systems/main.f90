! PROGRAM CREATED TO INTRO TO SCIENTIFIC COMPUTATION COURSE OF IME`S POST-GRADUATION ENGINEERING COURSE
! WRITTEN BY WALLACE RAMOS ROSENDO DA SILVA
! PROFESSOR ACHILES ARANTES BASSI
! EXERCISE 2D FROM PAGE 206 - RUGGIERO & LOPES' BOOK

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
    integer(fgsl_int) :: iter, nmax, unit1, itermax
    real(fgsl_double) :: start, finish
    real(fgsl_double), allocatable :: xvec(:),newton_result(:)

    call cpu_time(start)
    
    call get_command_argument(1,exercise) ! Rosembrock = '2d'; Broyden = '2e'
    call get_command_argument(2,jacobianoption) ! analytical (Newton); numerical (Discrete Newton); broyden (Broyden)
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
        print *, "Select a correct option: Broyden or Rosembrock. Try again."
	    stop
    end select 
    
    ! Non-Linear Newton's Resolution Method
    select case(jacobianoption)
    case('analytical')
        call newton(nmax, xvec, exercise, jacobianoption, linearmethod, newton_result, itermax)
    case('numerical') 
        call newton(nmax, xvec, exercise, jacobianoption, linearmethod, newton_result, itermax)
    case('broyden')
        call broyden(nmax, xvec, exercise, jacobianoption, linearmethod, newton_result, itermax)
    case default
        print *, "Select a correct option: Newton, Discrete Newton or Broyden. Try again."
        stop
    end select

    call cpu_time(finish)

    write(unit1,'(A)') "Number of iterations:"
    write(unit1,'(I10)') itermax
    write(unit1,'(A)') "CPU Time spent in the exercise "//trim(exercise)//" using "//trim(jacobianoption)//" with "&
    &//trim(linearmethod)//":"
    write(unit1,'(f8.5,a)') finish-start," seconds."
    write(unit1,'(A)') "Solution of the exercise "//trim(exercise)//" using "//trim(jacobianoption)//" with "&
    &//trim(linearmethod)//":"
    write(unit1,'(f8.5)') newton_result

    deallocate(xvec,newton_result)

end program main
