function A = vandermonde(x)
    i = int8(0);
    for i = 1 : size(x,1)
        A(:,i) = x.^(i-1);
    end
end