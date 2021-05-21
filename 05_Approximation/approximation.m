% Leitura dos dados do exercicio 
dados_questao = readtable('dados_interpol.csv');
% Leitura dos dados da aproximacao por codigo proprio em Fortran
dados_fortran = cell2mat(readcell('dados_aprox.dat'));
% Valores em x
xquestao = dados_questao.x;
% Valores em y
yquestao = dados_questao.y;
[ajuste,gof] = fit(xquestao, yquestao, 'exp1');
% Plotando grafico com aproximacao
vetor = linspace(2,40,101);
hold on
plot(xquestao,yquestao,'o',dados_fortran(:,1),dados_fortran(:,2),'b-')
legend()
plot(ajuste,'r-')
hold off