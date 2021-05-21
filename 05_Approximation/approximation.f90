!   Aproximacao nao linear para chegar ao resultado
!   y = a*e^(bx)
program approximation
    use fgsl 
    use mmq
    use direct_resolution_methods

    implicit none

    integer(fgsl_int) :: nmax = 17, unidadeleitura, unidadeescrita, rc1, rc2, i, divisoes = 101
    real(fgsl_double) :: razao, xinicial = 2.0, xfinal = 40.0, x_aprox, y_aprox, coefs(2)
    real(fgsl_double), allocatable :: dadosquestao(:,:), xquestao(:), yquestao(:)
    
    open(newunit=unidadeleitura, & ! Abrir o arquivo csv
    file='dados_interpol.csv', &
    action='read', &
    iostat=rc1 &
    )

    open(newunit=unidadeescrita, & ! Escrever neste arquivo
    file = 'dados_aprox.dat', &
    action = 'write', &
    iostat = rc2 &
    )

    ! Alocacao dinamica de memoria
    allocate(dadosquestao(nmax+1,2))
    allocate(xquestao(nmax))
    allocate(yquestao(nmax))

    if (rc1 /= 0) stop ! Caso haja problemas com o arquivo csv

    do i = 1, nmax + 1      ! Lendo os dados da tabela com os valores da questao
        read(unidadeleitura,*,iostat=rc1) dadosquestao(i,:)
    end do

    do i = 1, nmax      ! Extrair da tabela os vetores x e y
        xquestao(i) = dadosquestao(i+1,1)
        yquestao(i) = dadosquestao(i+1,2)
    end do
    
    coefs = 0.0d0

    call naolinear(xquestao,yquestao,nmax,coefs)

    razao = (xfinal-xinicial)/dble(divisoes-1) ! Razao para calcular o vetor de interesse a interpolar 
    do i = 1, divisoes                         ! Vetor linearmente espacado para calcular a spline
        x_aprox = xinicial + (i-1)*razao           ! X interpolado
        y_aprox = coefs(1)*exp(coefs(2)*x_aprox)   ! Y interpolado
        write(unidadeescrita,*) x_aprox, y_aprox 
    end do
    
    ! Fechando as unidades de escrita e leitura 
    close(unidadeleitura)
    close(unidadeescrita)
end program approximation