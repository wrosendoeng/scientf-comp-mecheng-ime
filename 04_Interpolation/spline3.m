function tabela = spline3(dataframe,vetor)
    
    x = dataframe.x;
    y = dataframe.y;
    n = length(x);
    
    h = x(2:end) - x(1:end-1);
    difdiv = (y(2:end)-y(1:end-1))./h;
    matrixdiff = zeros(n-2,n);
    
    for i = 1:n-2
        for j = 1:n
            switch i
                case j
                    matrixdiff(i,j) = h(i);
                case j - 1
                    matrixdiff(i,j) = 2*(h(i+1)+h(i));
                case j - 2
                    matrixdiff(i,j) = h(i+1);
            end
        end
    end
    
    vectordiff = 6*(difdiv(2:end)-difdiv(1:end-1));
    newmatrix = matrixdiff(:,2:end-1);
    % Spline cúbica natural (adicionando 0 nos extremos)
    v = plufactor(newmatrix,vectordiff,n-2); % Calculando fatoração LU
    g = [0;v;0];
    % Calculando os coeficientes com base em g(k)
    a = (g(2:end)-g(1:end-1))./(6*h);
    b = g/2.0;
    c = difdiv - h.*(g(2:end)+2*g(1:end-1))/6;
    d = y;
    
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
        s(k) = a(index)*(tabela.Value(k)-tabela.LeftEdge(k)).^3 ...
        +b(index)*(tabela.Value(k)-tabela.LeftEdge(k)).^2 ...
        +c(index)*(tabela.Value(k)-tabela.LeftEdge(k))+d(index);
    end
    
    % adicionar dados da interpolação na tabela:
    tabela.Spline3d = s.';
    
end