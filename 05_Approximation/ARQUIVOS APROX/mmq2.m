% MÉTODO DOS MÍNIMOS QUADRADOS para ajustar um polinômio do 1o grau
%
% Gera as matrizes A e b que fornecem a0 e a1
%
% DADOS DE ENTRADA (TABELA) --> 'x' e 'y'
%
% SAÍDA --> a1, a2 que geram c e d para y = c.e^(d.x)
%
% z = ln(y)
% z = ln(c) - d*x
% a1 = ln(c) e a2 = -d
% z = a1 + a2*x
% g1(x) = 1 e g2(x) = x
function [coef] = mmq(x,y)
%
% Cálculo das matrizes A e b
%
m = length(x);
a = zeros(2,2);
b = zeros(2,1);
z = log(y);

for k = 1 : m
    for i = 1 : 2
        if i == 1
            g(k,i) = 1;
        else
            g(k,i) = x(k);
        end
    end
end

for k = 1 : m
    for i = 1 : 2
        for j = 1 : 2
            a(i,j) = a(i,j) + g(k,i)*g(k,j);
        end
        b(i) = b(i) + z(k)*g(k,i);
    end
end
%
% Cálculo dos coeficientes a1 e a2;
%
alpha = inv(a)*b;
a1 = alpha(1);
a2 = alpha(2);
%
% Cálculo de c e d
%
c = exp(a1);
d = a2;
coef = [c,d]';
%
end 