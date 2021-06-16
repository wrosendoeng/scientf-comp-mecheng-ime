function [v,a] = accel_rk4(x,v,t)
m1 = 1; m2 = m1; m3 = m2;
% Amortecimento b = 0.3
b1 = 0.3; b2 = b1; b3 = b2;
% Constante das molas:
k1 = 1.0; k2 = k1; k3 = k2;
a(1) = (2*sin(t)-(b1+b2)*v(1) + b2*v(2) - (k1+k2)*x(1) + k2*x(2))/m1;
a(2) = (b2*v(1) -(b2+b3)*v(2) + b3*v(3) + k2*x(1) - (k2-k3)*x(2) + k3*x(3))/m2;
a(3) = (b2*v(2) - b3*v(3) + x(2)*k3 - k3*x(3))/m3;
end 
