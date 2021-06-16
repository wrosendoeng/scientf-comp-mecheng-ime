module mmq
    use fgsl
    use direct_resolution_methods, only: lufactorization
    implicit none
    contains

    subroutine naolinear(x,y,n,coef)
        !  MÉTODO DOS MÍNIMOS QUADRADOS para ajustar um polinômio do 1o grau
        !  Gera as matrizes A e b que donecem a0 e a1
        !  DADOS DE ENTRADA (TABELA) --> 'x' e 'y'
        !  SAÍDA --> a1, a2 que geram c e d para y = c.e^(d.x)
        !  z = ln(y)
        !  z = ln(c) - d*x
        !  a1 = ln(c) e a2 = -d
        !  z = a1 + a2*x
        !  g1(x) = 1 e g2(x) = x
        ! 
        !  Cálculo das matrizes A e b
        !
        integer(fgsl_int) :: i, j, k, n 
        real(fgsl_double) :: a(2,2), b(2), g(n,2), vetor(2)
        real(fgsl_double), intent(in) :: x(n), y(n)
        real(fgsl_double), intent(out) :: coef(2)
        ! Definindo os vetores de apoio para a aproximacao
        !
        a = 0.0d0;   ! Vetor a inicializado como 0 em todos os pontos
        b = 0.0d0;   ! Vetor b inicializado como 0 em todos os pontos
        !
        !
        do k = 1 , n
            do i = 1 , 2
                if (i == 1) then
                    g(k,i) = 1
                else
                    g(k,i) = x(k)
                end if
            end do
        end do

        do k = 1 , n
            do i = 1 , 2
                do j = 1 , 2
                    a(i,j) = a(i,j) + g(k,i)*g(k,j)
                end do 
                b(i) = b(i) + log(y(k))*g(k,i)
            end do
        end do
        !
        !  Cálculo dos coeficientes alpha1 e alpha2
        ! 
        call lufactorization(a,b,vetor,size(a,1))
        coef = (/exp(vetor(1)),vetor(2)/)

    end subroutine naolinear

    subroutine trigonometrica(x,y,n,coef)
        !  MÉTODO DOS MÍNIMOS QUADRADOS para ajustar um polinômio do 1o grau
        !  Gera as matrizes A e b que donecem a0 e a1
        !  DADOS DE ENTRADA (TABELA) --> 'x' e 'y'
        !  SAÍDA --> a1, a2 que geram c e d para y = c + d*sin(t)
        !  z = sin(t)
        !  w = a1 + a2*z
        !  a1 = a1 e a2 = -d
        !  z = a1 + a2*t
        !  g1(t) = 1 e g2(t) = t
        ! 
        !  Cálculo das matrizes A e b
        !
        integer(fgsl_int) :: i, j, k, n 
        real(fgsl_double) :: a(2,2), b(2), g(n,2), vetor(2)
        real(fgsl_double), intent(in) :: x(n), y(n)
        real(fgsl_double), intent(out) :: coef(2)
        ! Definindo os vetores de apoio para a aproximacao
        !
        a = 0.0d0;   ! Vetor a inicializado como 0 em todos os pontos
        b = 0.0d0;   ! Vetor b inicializado como 0 em todos os pontos
        !
        !
        do k = 1 , n
            do i = 1 , 2
                if (i == 1) then
                    g(k,i) = 1
                else
                    g(k,i) = sin(x(k))
                end if
            end do
        end do

        do k = 1 , n
            do i = 1 , 2
                do j = 1 , 2
                    a(i,j) = a(i,j) + g(k,i)*g(k,j)
                end do 
                b(i) = b(i) + y(k)*g(k,i)
            end do
        end do
        !
        !  Cálculo dos coeficientes alpha1 e alpha2
        ! 
        call lufactorization(a,b,vetor,size(a,1))
        coef = (/vetor(1),vetor(2)/)

    end subroutine trigonometrica
end module mmq