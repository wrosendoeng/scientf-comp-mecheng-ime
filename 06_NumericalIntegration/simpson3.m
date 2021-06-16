function [Iimpar,Esr] = simpson3(x,y)
    h = x(2) - x(1);
    m = (x(end)-x(1))/h;
    
    Isr = y(1) + y(end);
    Ipar = 0.0; Iimpar = Ipar;
    for i = 2:size(x,2)-1
        if (mod(i,2) == 0)
            Ipar = Ipar + y(i);
        else
            Iimpar = Iimpar + y(i); 
        end
    end
    
    % Integracao Numerica Resolvida
    Iimpar = h/3.0*(Isr+4*Ipar+2*Iimpar);
    % Erro associado
    Esr = m*h^5*max(y)/180;
end