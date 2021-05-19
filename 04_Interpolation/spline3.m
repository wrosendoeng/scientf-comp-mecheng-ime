function tabela = spline3(dataframe,vetor,n)
    
    x = dataframe.x;
    y = dataframe.y;
    
    for k = 1:n
        h(k) = x(k+1) - x(k);
    end
    
    for k = 1:n-1
        b(k) = 6*((y(k+2)-y(k+1))/h(k+1)-(y(k+1)-y(k))/h(k));
    end
    
    matrixdiff = zeros(n+1,n+1);
    matrixdiff(1,1) = 2.0; matrixdiff(end,end) = matrixdiff(1,1);
    matrixdiff(1,2) = 1.0; matrixdiff(end,end-1) = matrixdiff(1,2);

    for i = 2:n
        for j = 1:n+1
            switch i
                case j
                    matrixdiff(i,j) = h(i);
                case j - 1
                    matrixdiff(i,j) = 2*(h(i)+h(i-1));
                case j - 2
                    matrixdiff(i,j) = h(i-1);
            end
        end
    end
    
    % Spline cúbica natural (adicionando 0 nos extremos)
    g = plufactor(matrixdiff,b,n-1); % Calculando fatoração LU
    
    for k = 1:n % Calculando os coeficientes com base em g(k)
        a(k) = 3*(y(k)-y(k+1))+h(k)*(g(k+1)+g(k));
        b(k) = 3*(y(k+1)-y(k))-h(k)*(2*g(k)+g(k+1));
        c(k) = h(k)*g(k);
        d(k) = y(k);
    end
    
    % Criando uma tabela para delimitar qual spline para cada valor entre
    % 2 <= x <= 40
    edges = dataframe.x; % Listando os extremos da variável
    whichBin = discretize(vetor, edges); % Discretizando a amostragem
    left = edges(whichBin); % Valores mínimos do extremo
    right = edges(whichBin+1); % Valores máximos do extremo
    tabela = table(vetor, whichBin, left, right, ...
        'VariableNames', {'Value', 'BinNumber', 'LeftEdge', 'RightEdge'});
    
    for k = 1:length(vetor)
        index = tabela.BinNumber(tabela.Value==vetor(k));
        s(k) = a(index)*((tabela.Value(k)-tabela.RightEdge(k))/(tabela.RightEdge(k)-tabela.LeftEdge(k)))^3+ ...
            b(index)*((tabela.Value(k)-tabela.RightEdge(k))/(tabela.RightEdge(k)-tabela.LeftEdge(k)))^2+ ...
            c(index)*(tabela.Value(k)-tabela.RightEdge(k))/(tabela.RightEdge(k)-tabela.LeftEdge(k))+...
            d(index);
    end
    
    % adicionar dados da interpolação na tabela:
    tabela.Spline3d = s.';
    
end