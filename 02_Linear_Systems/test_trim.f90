<<<<<<< HEAD
PROGRAM read_matrix
    !CHARACTER(len=10), PARAMETER :: s = "GFORTRAN  "
    !WRITE(*,*) LEN(s), LEN(TRIM(s))  ! "10 8", with/without trailing blanks
    INTEGER(4) :: n_linhas, n_colunas
    REAL(4), ALLOCATABLE :: matriz(:,:)
    
    OPEN(unit = 10, action = 'read', status='old', file='matrices&vectors.txt', &
    & access ='sequential', form = 'formatted', recl = 100)

    ALLOCATE(matriz(n_linhas,n_colunas))
    do i = 1, n_linhas
        do j = 1, n_colunas
            print(10,'F10.8') matriz(n_linhas,n_colunas)
        end do
    end do 

    DEALLOCATE(matriz)

=======
PROGRAM read_matrix
    !CHARACTER(len=10), PARAMETER :: s = "GFORTRAN  "
    !WRITE(*,*) LEN(s), LEN(TRIM(s))  ! "10 8", with/without trailing blanks
    INTEGER(4) :: n_linhas, n_colunas
    REAL(4), ALLOCATABLE :: matriz(:,:)
    
    OPEN(unit = 10, action = 'read', status='old', file='matrices&vectors.txt', &
    & access ='sequential', form = 'formatted', recl = 100)

    ALLOCATE(matriz(n_linhas,n_colunas))
    do i = 1, n_linhas
        do j = 1, n_colunas
            print(10,'F10.8') matriz(n_linhas,n_colunas)
        end do
    end do 

    DEALLOCATE(matriz)

>>>>>>> 2bd4f61f96afd92a8f1564a188da886181568064
END PROGRAM