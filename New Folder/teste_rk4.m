clear all
clc
% Calculando o tempo
t = linspace(0,200,2001);
% Velocidade inicial
v = [0 0 0]; v_rk1 = v; v_rk2 = v; v_rk3 = v; v_rk4 = v;
% Aceleracao inicial
a = [0 0 0]; a_rk1 = a; a_rk2 = a; a_rk3 = a; a_rk4 = a;
%Posicao inicial:
x = [0 0 0];

% x' = v
% x'' = v'
% v(1)' = (2*sin(t)-(b1+b2)*v(1) + b2*v(2) - (k1+k2)*x(1) + k2*x(2))/m1
% v(2)' = (b2*v(1) -(b2+b3)*v(2) + b3*v(3) + k2*x(1) - (k2-k3)*x(2) + k3*x(3))/m2
% v(3)' = (b2*v(2) - b3*v(3) + x(2)*k3 - k3*x(3))/m3

% Calculando o passo:
time_step = (t(2)-t(1));

% Calculando o loop:
for i = 1:size(t,2)
    v_rk1 = time_step*accel_rk4(x(i),v(i),t(i));
    v_rk2 = time_step*accel_rk4(x(i)+0.5*time_step,v(i)+0.5*v_rk1,t(i));
    v_rk3 = time_step*accel_rk4(x(i)+0.5*time_step,v(i)+0.5*v_rk2,t(i));
    v_rk4 = time_step*accel_rk4(x(i)+1.0*time_step,v(i)+1.0*v_rk3,t(i));
    
    x(i+1) = x(i) + time_step/6*(v_rk1+2*v_rk2+2*v_rk3+v_rk4);
    position(i+1,:) = x(i+1)
    %v(i+1,:) = v(i,:) + time_step/6*(a_rk1+2*a_rk2+2*a_rk3+a_rk4);
end

disp("Deslocamento")
disp(x)
%disp("Velocidade")
%disp(v);
%Plotando o grafico de acordo com a exigencia do professor:
