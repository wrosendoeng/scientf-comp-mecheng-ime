function mmq_test(x,y)
    % Comparacao entre ajustes linear e quadratico
    % Caso linear:
    g = ones(size(x,2),2);
    g(:,2) = x;
    
    % Caso quadratico:
    h = ones(size(x,2),3);
    h(:,2) = x;
    h(:,3) = x.^2;
    
    % Caso linear
    coefs_linear = zeros(2,2);
    resut_linear = zeros(2,1);
    for k = 1:size(x,2)
        for i = 1:2
            for j = 1:2
                coefs_linear(i,j) = coefs_linear(i,j) + g(k,i)*g(k,j);
            end
            resut_linear(i) = resut_linear(i) + y(k)*g(k,i);
        end
    end
    
    % Caso quadratico
    coefs_quad = zeros(3,3);
    resut_quad = zeros(3,1);
    for k = 1:size(x,2)
        for i = 1:3
            for j = 1:3
                coefs_quad(i,j) = coefs_quad(i,j) + h(k,i)*h(k,j);
            end
            resut_quad(i) = resut_quad(i) + y(k)*h(k,i);
        end
    end
    
    % Resolver os sistemas lineares:
    alfa_linear = coefs_linear\resut_linear;
    alfa_quad = coefs_quad\resut_quad;
    
    % Gerar as curvas mmq para os casos linear e quadratico:
    phi_linear = alfa_linear(1) + alfa_linear(2)*x;
    phi_quad = alfa_quad(1) + alfa_quad(2)*x + alfa_quad(3)*x.^2;
    
    % Dispersoes em x e em y:
    disp_linear = phi_linear - y;
    disp_quad = phi_quad - y;
    
    % Figura 1: curvas comparadas com a funcao real
    figure(1)
    hold on
    plot(x,y,'b-')
    plot(x,phi_linear,'rx')
    plot(x,phi_quad,'go')
    title('Curvas MMQ ajustadas para f(x)')
    xlabel('x')
    ylabel('y')
    legend('f(x)','mmq linear','mmq quad','Location','northwest')
    hold off
    
    % Figura 2: analisar as dispersoes
    figure(2)
    hold on
    plot(x,disp_linear,'ko')
    plot(x,disp_quad,'rx')
    title('Grafico de Dispersoes')
    xlabel('x')
    ylabel('y')
    legend('Disp linear','Disp quad','Location','northwest')
    hold off
end 