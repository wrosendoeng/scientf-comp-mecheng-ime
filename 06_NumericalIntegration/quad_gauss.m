function I = quad_gauss()
% integral of exp(-x^2) de 0 a 1
%   onde: 
%   intervalo inicial = [0 1]
%   x = 0.5*(t+1)
%   dx = 0.5*dt
%   f(x) = exp(-x^2)
%   g(t) = exp(-0.25*(t+1)^2)

a1 = 1.0; a2 = a1; 
t = [-1 1];
g = exp(-(t+1.0).^2/4.0);
I = 0.5*(a1*g(1)+a2*g(2));

end 