% % Dados da questao no arquivo XLSX
dados_aprox = readtable('dados_interpol.csv');
% Convertendo para array
mat_aprox = table2array(dados_aprox);
xvalores = linspace(2,40,100);
% % % Interpolacao Lagrangiana
for i = 1 : 100
    funcao_interpol(i) = lagrangian(xvalores(i),mat_aprox);
end
% % Armazendo os dados
dados_matlab_fgsl = table(xvalores.',funcao_interpol.',...
    'VariableNames',{'x_matlab','y_matlab'});
% % Plotando as curvas para analisar as diferencas
plot(xvalores,funcao_interpol,'o') %,vetor,interpol,'r+',...
%     vetor,dados_fgsl(:,2),'b-',vetor,dados_fortran(:,2),'k--')
% xlabel('x')
% ylabel('y')
% title('Interpolacao - trabalho 04')
% legend('questao','matlab','fgsl','fortran')
% % Salvando os dados em um arquivo a parte
% writetable(dados_matlab_fgsl,'dados_matlab_fgsl.dat')

