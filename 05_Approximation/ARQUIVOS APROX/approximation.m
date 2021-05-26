% Leitura dos dados do exercicio 
dados_questao = readtable('dados_interpol.csv');
% Leitura dos dados da aproximacao por codigo proprio em Fortran
dados_fgsl = cell2mat(readcell('dados_aprox_fgsl.dat'));
dados_fortran = cell2mat(readcell('dados_aprox.dat'));
% Valores em x
xquestao = dados_questao.x;
% Valores em y
yquestao = dados_questao.y;
coefs = mmq2(xquestao, yquestao);
% Media da amostragem principal
yqmedia = mean(yquestao);           
% Resultado do coeficiente de determinacao da Regressão
aprox = coefs(1)*exp(xquestao*coefs(2));
% Soma dos quadrados residuos 
sse_matlab = sum((yquestao-aprox).^2);                  % MATLAB
sse_fgsl = sum((yquestao-dados_fgsl(:,2)).^2);          % FGSL
sse_fortran = sum((yquestao-dados_fortran(:,2)).^2);    % Fortran
% Soma dos quadrados totais
sst = sum((yquestao-yqmedia).^2);
% Coeficiente de determinacao R2 para cada implementacao
r2_matlab = 1 - sse_matlab/sst;
r2_fgsl = 1 - sse_fgsl/sst;
r2_fortran = 1 - sse_fortran/sst;
% Plotando grafico com aproximacao
vetor = transpose(linspace(2,40,100));
nlin_regmat = coefs(1)*exp(vetor*coefs(2));
hold on
plot(xquestao,yquestao,'o',vetor,nlin_regmat,'r-.',...
    vetor,dados_fgsl(:,2),'b--',vetor,dados_fortran(:,2),'k-')
xlabel('x')
ylabel('y')
title('Aproximacao - trabalho 04')
legend('questao','matlab','fgsl','fortran')
hold off
% Armazendo os dados do MATLAB com FGSL e Fortran
dados_matlab_fgsl_aprox = table(vetor,nlin_regmat,dados_fgsl(:,2),dados_fortran(:,2),...
    'VariableNames',{'x_matlab','y_matlab','y_fgsl','y_fortran'});