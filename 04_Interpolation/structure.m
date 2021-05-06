% CODE CREATED BY WALLACE RAMOS ROSENDO DA SILVA
% INTERPOLATION METHODS USING MATLAB
% INTRODUCTION TO SCIENTIFIC COMPUTATION - BRAZILIAN ARMY'S MILITARY INSTITUTE OF ENGINEERING (IME)
% POST-GRADUATION COURSE - MECHANICAL ENGINEERING (PGMEC - SE/4)
% PROFESSOR ACHILLES ARANTES BASSI

%% DEFINING X AND Y RANGES FOR STUDY INTEREST:
% nodes(:,1) = x values
% nodes(:,2) = y values
% points = range of interest for x values

nodes(:,1) = [1.59,  4.46,  7.32, 10.29, 13.31, 16.28, 19.33,...
    22.40, 25.29, 28.17, 31.21, 34.32, 37.22, 40.20,...
    43.25, 46.25, 49.20];
nodes(:,2) = [92.75, 79.16, 76.56, 67.39, 65.92, 61.00, 50.76,...
     49.86, 43.83, 40.74, 38.44, 34.87, 30.39, 28.70,...
     25.88, 22.72, 21.48];
points = 2:0.01:40;

%% POLYNOMIAL INTERPOLATION - DISCRETE CASE:
% First form: Solving a Linear Algebra System Ax = b
% where A = Vandermonde's matrix of xi, from i = 0 to i = n
%       x = coefficients of polynomial interpolation
%       b = exact solution of f(xi)
% In that case, it will be solved using PLU Factorization:
domin_values = vandermonde(nodes(:,1));
coef_plu = plufactor(domin_values,nodes(:,2),size(nodes,1),'double');
y_plu = 
% Second Form: Lagrangian Interpolation
% where pn(x) = summation of y(k)Lk(x), in from k = 1 to n + 1
%       Lk(x) = productory of (x - x(j))/(x(k)-x(j)) from j = 1 to n + 1, if j /= k
y_lag = lagrangian(nodes(:,1),nodes(:,2));
% Third Form: Newton's Form
% where pn(x) = f(x0) + (x-x0)f[x0,x1] + ... + (x-x0)(x-x1)...
%       ...(x-xn-1)*f[x0,x1,...,xn]
%       En(x) = (x-x0)(x-x1)...(x-xn)*f[x0,x1,...xn,x]
%       f[x0,x1,...,xn] = divided-differences methods





