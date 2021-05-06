function yp = lagrangian(x,y,xp)

    j = int8(0); k = j;
    n = size(x,1);
    yp = 0;

    for k = 1 : n
        pr = 1.0;
        for j = 1 : n
            if j ~= k
                pr = pr.*(xp-x(j,1))/(x(k,1)-x(j,1));
            end
        end
        yp = yp + pr*y(k,1);
    end

end