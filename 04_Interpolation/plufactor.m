% PLU FACTORIZATION
function x = plufactor(a,b,n)
% Defining variables
aux = int16(0); i = aux; j = i; k = j; l_pivot = k;
p = zeros(1,n,'int16'); % precision = int8 or int16
m = double(0); pivot = m; swap = pivot; % Single or double precision
c = zeros(n,1); y = c; x = y;
    
    % Pivoting
    for i = 1 : n 
        p(i) = i; 
    end
    
    for k = 1 : n-1
        pivot = a(k,k); 
        l_pivot = k ;
        for i = (k+1) : n
            if abs(a(i,k)) > abs(pivot)
                pivot = a(i,k);
                l_pivot = i ;
            end
        end
        if (pivot == 0.0) 
            break
        end 
        if l_pivot ~= k
            troca = b(k);
            b(k) = b(l_pivot);
            b(l_pivot) = troca;
            for j = 1 : n
                swap = a(k,j);
                a(k,j) = a(l_pivot,j);
                a(l_pivot,j) = swap;
            end
        end
        for i = (k+1) : n
            m = a(i,k)/a(k,k);
            a(i,k) = 0.0;
            for j = (k+1) : n
                a(i,j) = a(i,j) - m*a(k,j);
            end
            b(i) = b(i) - m*b(k);
        end
    end
    
    % c = Pb
    for i = 1 : n
        aux = p(i);
        c(i) = b(aux);
    end
    
    % Ly = Pb
    for i = 1 : n
        for j = 1 : n-1
            y(i) = y(i) + a(i,j)*y(j);
        end
        y(i) = c(i) - y(i);
    end
    
    % Ux = y
    for i = n : -1 : 1   
        for j = (i+1) : n
            x(i) = x(i) + a(i,j)*x(j);
        end
        x(i) = (y(i)-x(i))/a(i,i);
    end
end