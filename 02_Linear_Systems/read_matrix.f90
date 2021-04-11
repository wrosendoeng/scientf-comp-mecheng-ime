PROGRAM linearsystems

    use create, only: matrizes_ab
    use direct_resolution_methods, only: lufactorization

    integer(4) :: cols,rows, numrows=17, aunit, bunit, cunit
    real(8), allocatable, dimension(:,:) :: matriza
    real(8), allocatable, dimension(:) :: vectorb, result
    
    ! open(newunit = aunit, action = 'read', status='old', file='matrizA.txt', &
    ! & access ='sequential', form = 'formatted')
    ! open(newunit = bunit, action = 'read', status='old', file='matrizB.txt', &
    ! & access ='sequential', form = 'formatted')
    open(newunit = cunit, action = 'write', status='replace', file='result.txt', &
    & access ='sequential', form = 'formatted')

    allocate(matriza(numrows,numrows))
    allocate(vectorb(numrows)) 
    allocate(result(numrows))

    ! read(aunit,*) matriza
    ! read(bunit,*) vectorb
    call matrizes_ab(matriza,vectorb)

    ! call elimination(matriza,vectorb,result,numrows)
    call lufactorization(matriza,vectorb,result,numrows)

    do rows = 1, numrows
        write(cunit,*) result(rows)
    end do
    
    deallocate(matriza)
    deallocate(vectorb)
    deallocate(result)
     
END PROGRAM linearsystems


