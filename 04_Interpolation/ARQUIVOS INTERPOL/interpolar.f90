program interpolar
    use fgsl
    use cubicspline
    use direct_resolution_methods
    implicit none

    integer(fgsl_int) :: nmax = 17
    integer(fgsl_int) :: unidadeleitura, unidadeescrita, rc, i, divisoes = 101
    real(fgsl_double) :: razao, xinicial = 2.0, xfinal = 40.0, x_interp, y_interp 
    real(fgsl_double), allocatable :: dadosquestao(:,:), xquestao(:), yquestao(:)

    open(newunit=unidadeleitura, & ! Abrir o arquivo csv
        file='dados_interpol.csv', &
        action='read', &
        iostat=rc &
    )

    open(newunit=unidadeescrita, & ! Escrever neste arquivo
        file = 'dados_interp.dat', &
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

    razao = (xfinal-xinicial)/dble(divisoes-1) ! Razao para calcular o vetor de interesse a interpolar 
    do i = 1, divisoes                         ! Vetor linearmente espacado para calcular a spline
        x_interp = xinicial + (i-1)*razao                         ! X interpolado
        y_interp = spline3(xquestao,yquestao,nmax,x_interp)       ! Y interpolado
        write(unidadeescrita,'(f10.6,5X,f10.6)') x_interp, y_interp 
    end do

    ! Desalocando da memoria os vetores
    deallocate(dadosquestao)
    deallocate(xquestao)
    deallocate(yquestao)
    
    ! Fechando as unidades de escrita e leitura 
    close(unidadeleitura)
    close(unidadeescrita)
    
end program interpolar