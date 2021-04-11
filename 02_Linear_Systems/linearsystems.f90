PROGRAM linearsystems

    use create, only: matrices_ab
    use direct_resolution_methods

    character(len=32) :: arg
    integer(4) :: iter ,rows, numrows=17, unit1
    real(8), allocatable, dimension(:,:) :: matriza
    real(8), allocatable, dimension(:) :: vectorb, result

    open(newunit = unit1, action = 'write', status='replace', file='result.txt', &
    & access ='sequential', form = 'formatted')

    allocate(matriza(numrows,numrows))
    allocate(vectorb(numrows)) 
    allocate(result(numrows))

    call matrices_ab(matriza,vectorb)
    call get_command_argument(iter, arg)

    if (arg == 'elimination') then
        call elimination(matriza,vectorb,result,numrows)
    else
        call lufactorization(matriza,vectorb,result,numrows)
    end if 

    do rows = 1, numrows
        write(unit1,*) result(rows)
    end do
    
    deallocate(matriza)
    deallocate(vectorb)
    deallocate(result)
     
END PROGRAM linearsystems


