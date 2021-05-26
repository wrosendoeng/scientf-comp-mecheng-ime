program fitting_fgsl
     use fgsl
     
     implicit none
     
     integer(fgsl_size_t), parameter :: nmax = 17
     integer(fgsl_int) :: status, unidadeleitura, unidadeescrita, rc1, rc2, i, divisoes = 100
     real(fgsl_double), allocatable :: xquestao(:), yquestao(:), zquestao(:),dadosquestao(:,:)
     real(fgsl_double) :: razao, xinicial = 2.0, xfinal = 40.0, x_aprox, y_aprox, z_aprox, coefs(2)
     real(fgsl_double) :: alpha0, alpha1, cov00, cov01, cov11, chisq, ymean
     real(fgsl_double) :: zaprox_err, yaprox_err

     open(newunit=unidadeleitura, & ! Abrir o arquivo csv
     file='dados_interpol.csv', &
     action='read', &
     iostat=rc1 &
     )

     open(newunit=unidadeescrita, & ! Escrever neste arquivo
     file = 'dados_aprox_fgsl.dat', &
     action = 'write', &
     iostat = rc2 &
     )
     
     ! Alocacao dinamica de memoria
     allocate(dadosquestao(nmax+1,2))
     allocate(xquestao(nmax))
     allocate(yquestao(nmax))
     allocate(zquestao(nmax))

     if (rc1 /= 0) stop ! Caso haja problemas com o arquivo csv

     do i = 1, nmax + 1      ! Lendo os dados da tabela com os valores da questao
          read(unidadeleitura,*,iostat=rc1) dadosquestao(i,:)
     end do

     do i = 1, nmax      ! Extrair da tabela os vetores x e y
          xquestao(i) = dadosquestao(i+1,1)
          yquestao(i) = dadosquestao(i+1,2)
     end do
     zquestao = log(yquestao)

     status = fgsl_fit_linear (xquestao, 1_fgsl_size_t, zquestao, 1_fgsl_size_t, nmax, & 
          alpha0, alpha1, cov00, cov01, cov11, chisq)

     razao = (xfinal-xinicial)/dble(divisoes-1) ! Razao para calcular o vetor de interesse a interpolar 
     do i = 1, divisoes                         ! Vetor linearmente espacado para calcular a spline
          x_aprox = xinicial + (i-1)*razao      ! X interpolado
          status = fgsl_fit_linear_est (x_aprox, alpha0, alpha1, cov00, & 
          cov01, cov11, z_aprox, zaprox_err)    ! Y interpolado
          y_aprox = exp(z_aprox)
          yaprox_err = exp(zaprox_err)
          write(unidadeescrita,'(F10.6,5X,F10.6)') x_aprox, y_aprox
     end do

     ! print *, exp(alpha0), alpha1
     ! Desalocacao de matrizes
     deallocate(dadosquestao)
     deallocate(xquestao)
     deallocate(yquestao)
     deallocate(zquestao)
     ! Fechando as unidades de escrita e leitura 
     close(unidadeleitura)
     close(unidadeescrita)
end program fitting_fgsl
