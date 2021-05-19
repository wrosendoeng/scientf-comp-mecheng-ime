% Dados da questao no arquivo XLSX
dados_fx = readtable('dados_interpol.xlsx');
% Dados obtidos da interpolacao com Fortran FGSL
dados_fgsl = cell2mat(readcell('tabelacompleta.dat'));
% Vetor a ser usado na interpolacao
vetor = transpose(linspace(2,40,length(dados_fgsl)));
% Interpolacao spline cubica
interpol = spline(dados_fx.x,dados_fx.y,vetor);
% Armazendo os dados do MATLAB com FGSL
dados_matlab_fgsl = table(vetor,interpol,dados_fgsl(:,2),...
    'VariableNames',{'x_matlab','y_matlab','y_fgsl'});
% Plotando as curvas para analisar as diferencas
plot(dados_fx.x,dados_fx.y,'o',vetor,interpol,'r-',...
    vetor,dados_fgsl(:,2),'b-')
% Salvando os dados em um arquivo a parte
writetable(dados_matlab_fgsl,'dados_matlab_fgsl.dat')