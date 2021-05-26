program interpolar_fgsl
    use fgsl
    implicit none

    integer(fgsl_size_t) :: nmax = 100
    integer(fgsl_int) :: unidadeleitura, unidadeescrita, rc, i, status, divisoes = 101
    real(fgsl_double) :: razao, xinicial = 2.0, xfinal = 40.0, x_interp, y_interp 
    real(fgsl_double), allocatable :: dadosquestao(:,:), xquestao(:), yquestao(:)
    type(fgsl_interp_accel) :: accel
    type(fgsl_interp) :: polynomial

    open(newunit=unidadeleitura, & ! Abrir o arquivo csv
        file='dados_aprox_fgsl.dat', &
        action='read', &
        iostat=rc &
    )

    open(newunit=unidadeescrita, & ! Escrever neste arquivo
        file = 'interp_aprox_fgsl.dat', &
        action = 'write', &
        iostat = rc &
    )

    ! Alocacao dinamica de memoria
    allocate(dadosquestao(nmax,2))
    allocate(xquestao(nmax))
    allocate(yquestao(nmax))

    if (rc /= 0) stop ! Caso haja problemas com o arquivo csv

    ! Lendo os dados da tabela com os valores da questao
    do i = 1, nmax
    read(unidadeleitura,*,iostat=rc) dadosquestao(i,:)
    end do

	! Extrair da tabela os vetores x e y
    xquestao = dadosquestao(:,1)
    yquestao = dadosquestao(:,2)    

    accel = fgsl_interp_accel_alloc()   ! Ponteiro para alocar unidade na memoria
    polynomial = fgsl_interp_alloc(fgsl_interp_polynomial, nmax) ! Ponteiro para alocar a funcao na memoria 
    status = fgsl_interp_init(polynomial, xquestao, yquestao) ! Iniciar o calcular da interpolacao com spline

    razao = (xfinal-xinicial)/dble(divisoes-1) ! Razao para calcular o vetor de interesse a interpolar 
    do i = 1, divisoes                         ! Vetor linearmente espacado para calcular a spline
        x_interp = xinicial + (i-1)*razao                                          ! X interpolado
        y_interp = fgsl_interp_eval(polynomial,xquestao,yquestao,x_interp,accel)   ! Y interpolado
        write(unidadeescrita,'(f10.6,5X,f30.2)') x_interp, y_interp 
    end do

    ! Desalocando da memoria os vetores
    deallocate(dadosquestao)
    deallocate(xquestao)
    deallocate(yquestao)
    
    call fgsl_interp_free(polynomial)      ! Liberando a funcao
    call fgsl_interp_accel_free(accel)  ! Liberando o ponteiro que permite realizar a interpolacao

    ! Fechando as unidades de escrita e leitura 
    close(unidadeleitura)
    close(unidadeescrita)
end program