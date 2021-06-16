!   Aproximacao nao linear para chegar ao resultado
!   y = a*e^(bx)
program approximation
    use fgsl 
    use mmq
    use direct_resolution_methods

    implicit none

    character(len=40) :: input_name, output_name
    integer(fgsl_int) :: nmax = 401, unidadeleitura, unidadeescrita, rc1, rc2, i
    real(fgsl_double) :: razao, xinicial, xfinal, x_aprox, y_aprox, coefs(2)
    real(fgsl_double), allocatable :: dadosquestao(:,:), xquestao(:), yquestao(:)

    call get_command_argument(1,input_name)  ! Entrada
    call get_command_argument(2,output_name) ! Saida

    open(newunit=unidadeleitura, & ! Abrir o arquivo csv pela linha de comando
    file=trim(input_name), &
    action='read', &
    iostat=rc1 &
    )

    open(newunit=unidadeescrita, & ! Escrever neste arquivo chamado na linha de comando
    file = trim(output_name), &
    action = 'write', &
    iostat = rc2 &
    )

    ! Alocacao dinamica de memoria
    allocate(dadosquestao(nmax,2),xquestao(nmax),yquestao(nmax))

    if (rc1 /= 0) stop ! Caso haja problemas com o arquivo csv

    do i = 1, nmax    ! Lendo os dados da tabela com os valores da questao
        read(unidadeleitura,*,iostat=rc1) dadosquestao(i,:)
        xquestao(i) = dadosquestao(i,1)
        yquestao(i) = dadosquestao(i,2)
    end do

    coefs = 0.0d0

    ! call naolinear(xquestao,yquestao,nmax,coefs)      ! Ajuste exponencial
    call trigonometrica(xquestao,yquestao,nmax,coefs)   ! Ajuste por seno

    razao = xquestao(2)-xquestao(1)             ! Razao para calcular o vetor de interesse a interpolar 
    do i = 1, nmax                          ! Vetor linearmente espacado para calcular a spline
        x_aprox = xquestao(1) + (i-1)*razao          ! X interpolado
        y_aprox = coefs(2)*sin(x_aprox) + coefs(1)   ! Y interpolado
        write(unidadeescrita,'(f12.6,5X,f12.6)') x_aprox, y_aprox 
    end do

    ! Desalocacao de matrizes
    deallocate(dadosquestao,xquestao,yquestao)
    
    ! Fechando as unidades de escrita e leitura 
    close(unidadeleitura)
    close(unidadeescrita)
end program approximation
