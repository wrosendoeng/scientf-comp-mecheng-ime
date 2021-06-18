program interpolar
    use fgsl
    use cubicspline
    use direct_resolution_methods
    implicit none

    character(len=40) :: input_name, output_name
    integer(fgsl_int) :: nmax = 401
    integer(fgsl_int) :: unidadeleitura2, unidadeescrita2, rc1, rc2, i
    real(fgsl_double) :: razao, x_interp, y_interp 
    real(fgsl_double), allocatable :: dadosquestao(:,:), xquestao(:), yquestao(:)

    call get_command_argument(1,input_name)
    call get_command_argument(2,output_name)
    
    open(newunit=unidadeleitura2, & ! Abrir o arquivo csv
        file=trim(input_name), &
        action='read', &
        iostat=rc1 &
    )

    open(newunit=unidadeescrita2, & ! Escrever neste arquivo
        file = trim(output_name), &
        action = 'write', &
        iostat = rc2 &
    )

    ! Alocacao dinamica de memoria
    allocate(dadosquestao(nmax,2),xquestao(nmax),yquestao(nmax))

    if (rc1 /= 0) stop ! Caso haja problemas com o arquivo csv

    do i = 1, nmax      ! Lendo os dados da tabela com os valores da questao
        read(unidadeleitura2,*,iostat=rc1) xquestao(i), yquestao(i)
    end do

    ! y_interp = 0.0d0
    razao = (xquestao(2)-xquestao(1))           ! Razao para calcular o vetor de interesse a interpolar 
    do i = 1, nmax                          ! Vetor linearmente espacado para calcular a spline
        x_interp = xquestao(1) + (i-1)*razao                ! X interpolado
        y_interp = spline3(xquestao,yquestao,nmax,x_interp) ! Y interpolado
        write(unidadeescrita2,'(f12.6,5X,f12.6)') x_interp, y_interp 
    end do

    ! Desalocando da memoria os vetores
    deallocate(dadosquestao,xquestao,yquestao)
    
    ! Fechando as unidades de escrita e leitura 
    close(unidadeleitura2)
    close(unidadeescrita2)
end program interpolar