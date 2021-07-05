module cubicspline
    use parameters, only: wp => PARM_DP, intlength => PARM_SI, fnl => PARM_SCL
    use direct_resolution_methods
    implicit none
    
    contains
    function spline3(x,y,n,valor) result(s)
        
        integer(intlength) :: i, j, k, index
        real(wp) :: g(n), h(n-1), matrixdiff(n-2,n), vectordiff(n-1), v(n-2), subdivs(n-1,3), &
         limesq, newmatrix(n-2,n-2), a(n-1), b(n-1), c(n-1), d(n), difdiv(n-1), x1(n-1), x2(n-1), bins(n-1)
        integer(intlength), intent(in) :: n
        real(wp), intent(in) :: x(n), y(n), valor
        real(wp) :: s
        
        h = x(2:n) - x(1:n-1)
        difdiv = (y(2:n)-y(1:n-1))/h
        matrixdiff = 0.0_wp
        
        do i = 1,n-2
            do j = 1,n
                if (i == j) then
                    matrixdiff(i,j) = h(i)
                else if (i == j - 1) then
                    matrixdiff(i,j) = 2*(h(i+1)+h(i))
                else if (i == j - 2) then
                    matrixdiff(i,j) = h(i+1)
                end if
            end do  
        end do
        
        vectordiff = 6*(difdiv(2:n)-difdiv(1:n-1))
        newmatrix = matrixdiff(:,2:n-1)
        ! Spline cúbica natural (adicionando 0 nos extremos)
        call lufactorization(newmatrix,vectordiff,v,n-2) ! Calculando fatoração LU
        g = (/0.0_wp,v,0.0_wp/)
        ! Calculando os coeficientes com base em g(k)
        a = (g(2:n)-g(1:n-1))/dble(6*h)
        b = g/2.0_wp
        c = difdiv - h*(g(2:n)+2*g(1:n-1))/6.0_wp
        d = y
        
        ! Criando uma matriz para delimitar os intervalos de aplicacao da spline
        do i = 1,n-1
            subdivs(i,1) = i
            subdivs(i,2) = x(i)
            subdivs(i,3) = x(i+1)
        end do
        
        bins = int(subdivs(:,1))
        x1 = subdivs(:,2)
        x2 = subdivs(:,3)
        ! Procurar o index que contem o range em que a variavel "valor" esta presente
        ! ex: valor = 2.0 ==> x1 = 1.59 e x2 = 4.46 ==> index = 1 (1o intervalo da questao)
        ! limesq = o valor menor do intervalo (por isso na esquerda)
        do i = 1, n-1
            if (valor .ge. x1(i) .and. valor .le. x2(i)) then
                index = bins(i)
                limesq = x1(i)
            end if
        end do
        ! Calculando o polinomio interpolador de acordo com o intervalo
        s = a(index)*(valor-limesq)**3 + b(index)*(valor-limesq)**2 & 
        + c(index)*(valor-limesq) + d(index)
        
    end function spline3
    
end module cubicspline