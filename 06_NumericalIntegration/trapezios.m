function [Itr, Etr] = trapezios(x,y)
    h = x(2) - x(1);
    m = (x(end)-x(1))/h;
    
    Itr = y(1) + y(end);
    for i = 2:size(x,2)-1
        Itr = Itr + 2*y(i); 
    end
    % Integracao Numerica Resolvida
    Itr = 0.5*h*Itr;
    % Erro associado
    Etr = m*h^3*max(y)/12;
    
end 