program interpolar_fgsl
    use fgsl
    implicit none

    integer(fgsl_size_t) :: nmax = 17
    integer(fgsl_int) :: unidadeleitura, unidadeescrita, rc, i, status, divisoes = 101
    real(fgsl_double) :: razao, xinicial = 2.0, xfinal = 40.0, x_interp, y_interp 
    real(fgsl_double), allocatable :: dadosquestao(:,:), xquestao(:), yquestao(:)
    type(fgsl_interp_accel) :: accel
    type(fgsl_spline) :: cspline

    open(newunit=unidadeleitura, & ! Abrir o arquivo csv
        file='dados_interpol.csv', &
        action='read', &
        iostat=rc &
    )

    open(newunit=unidadeescrita, & ! Escrever neste arquivo
        file = 'dados_fgsl_interp.dat', &
        action = 'write', &
        iostat = rc &
    )

    ! Alocacao dinamica de memoria
    allocate(dadosquestao(nmax+1,2))
    allocate(xquestao(nmax))
    allocate(yquestao(nmax))

    if (rc /= 0) stop ! Caso haja problemas com o arquivo csv

    do i = 1, nmax + 1      ! Lendo os dados da tabela com os valores da questao
        read(unidadeleitura,*,iostat=rc) dadosquestao(i,:)
    end do

    do i = 1, nmax      ! Extrair da tabela os vetores x e y
        xquestao(i) = dadosquestao(i+1,1)
        yquestao(i) = dadosquestao(i+1,2)
    end do   

    accel = fgsl_interp_accel_alloc()   ! Ponteiro para alocar unidade na memoria
    cspline = fgsl_spline_alloc(fgsl_interp_cspline, nmax) ! Ponteiro para alocar a funcao na memoria 
    status = fgsl_spline_init(cspline, xquestao, yquestao) ! Iniciar o calcular da interpolacao com spline

    razao = (xfinal-xinicial)/dble(divisoes-1) ! Razao para calcular o vetor de interesse a interpolar 
    do i = 1, divisoes                         ! Vetor linearmente espacado para calcular a spline
        x_interp = xinicial + (i-1)*razao                     ! X interpolado
        y_interp = fgsl_spline_eval(cspline,x_interp,accel)   ! Y interpolado
        write(unidadeescrita,'(f10.6,5X,f10.6)') x_interp, y_interp 
    end do

    ! Desalocando da memoria os vetores
    deallocate(dadosquestao)
    deallocate(xquestao)
    deallocate(yquestao)
    
    call fgsl_spline_free(cspline)      ! Liberando a funcao
    call fgsl_interp_accel_free(accel)  ! Liberando o ponteiro que permite realizar a interpolacao

    ! Fechando as unidades de escrita e leitura 
    close(unidadeleitura)
    close(unidadeescrita)
end program