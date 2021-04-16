PROGRAM linearsystems

    use create, only: matrices_ab
    use direct_resolution_methods
    use iterative_resolution_methods

    character(len=32) :: arg
    integer(4) :: rows, numrows=17, unit1
    real(8), allocatable, dimension(:,:) :: matriza
    real(8), allocatable, dimension(:) :: vectorb, result

    allocate(matriza(numrows,numrows))
    allocate(vectorb(numrows)) 
    allocate(result(numrows))

    call matrices_ab(matriza,vectorb,numrows)
    call get_command_argument(1, arg)

    select case (arg)
        case('elimination')
            open(newunit = unit1, action = 'write', status='replace', file=trim(arg)//'.txt', &
            & access ='sequential', form = 'formatted')
            call elimination(matriza,vectorb,result,numrows)
        case('plu')
            open(newunit = unit1, action = 'write', status='replace', file=trim(arg)//'.txt', &
            & access ='sequential', form = 'formatted')
            call lufactorization(matriza,vectorb,result,numrows)
        case('seidel')
            open(newunit = unit1, action = 'write', status='replace', file=trim(arg)//'.txt', &
            & access ='sequential', form = 'formatted')
            call gauss_seidel(matriza,vectorb,result,numrows)
        case('jacobi')
            open(newunit = unit1, action = 'write', status='replace', file=trim(arg)//'.txt', &
            & access ='sequential', form = 'formatted')
            call gauss_jacobi(matriza,vectorb,result,numrows)
        case default
            print *, "You haven't chosen any option. Try again"
    end select

    do rows = 1, numrows
        write(unit1,*) result(rows)
    end do
    
    deallocate(matriza)
    deallocate(vectorb)
    deallocate(result)
     
END PROGRAM linearsystems


