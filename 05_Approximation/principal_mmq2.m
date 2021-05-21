%
% y = a*e^(bx)
%
x = [0.3 0.7 1.2 2.0 2.3 3.0 3.9 4.1 4.6 5.2 6.0 6.1];
y = [58.1 54.9 53.7 39.8 40.9 28.2 17.8 15.8 9.1 6.0 4.5 2.8];
n = length(x);
%
r1 = 98.9876;
r2 = -0.5122;
%
sol = [r1 r2]';
tol = 1.e-4;
%
[coef] = trabalho2_mmq2(x,y)
%
res = norm(sol - coef);
%
if res < tol
erro = 0
else
erro = 1
end