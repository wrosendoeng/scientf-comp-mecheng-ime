% Dados da questao no arquivo XLSX
dados_fx = readtable('dados_interpol.csv');
% Dados obtidos da interpolacao com Fortran FGSL
%dados_fgsl = cell2mat(readcell("dados_fgsl_interp.dat"));
%dados_fortran = cell2mat(readcell("dados_interp.dat"));
% Vetor a ser usado na interpolacao
vetor = transpose(linspace(2,40,height(dados_fx)));
% Interpolacao spline cubica
interpolframe = spline3(dados_fx,vetor);
interpol = interpolframe.Spline3d;
% Armazendo os dados do MATLAB com FGSL
%dados_matlab_fgsl = table(vetor,interpol,dados_fgsl(:,2),dados_fortran(:,2),...
%    'VariableNames',{'x_matlab','y_matlab','y_fgsl','y_fortran'});
% Plotando as curvas para analisar as diferencas
%plot(dados_fx.x,dados_fx.y,'o',vetor,interpol,'r+',...
%    vetor,dados_fgsl(:,2),'b-',vetor,dados_fortran(:,2),'k--')
%xlabel('x')
%ylabel('y')
%title('Interpolacao - trabalho 04')
% legend('questao','matlab','fgsl','fortran')
% Salvando os dados em um arquivo a parte
% writetable(dados_matlab_fgsl,'dados_matlab_fgsl.dat')