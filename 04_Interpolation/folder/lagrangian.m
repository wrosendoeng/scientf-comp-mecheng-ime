function [yp,xp] = lagrangian(xp,dataframe)
    x = dataframe(:,1);
    y = dataframe(:,2);
    n = length(x);
    yp = 0;
    
    for i = 1 : n
        p = 1;
        for j = 1 : n
            if i ~= j 
                p = p*(xp-x(j))/(x(i)-x(j));
            end
        end
        yp = yp + p*y(i);
    end 
end