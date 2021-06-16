function y1 = metodo_euler(x0,x1,y0)
h = 0.05;
for x = x0:h:x1-h
    y1 = y0 + h*func(x,y0);
    y0 = y1;
end
end

function y2 = metodo_heun(x0,x1,y0)
h = 0.05;
for x = x0:h:x1-h
    y1 = y0 + h/2*func(x,y0);
    y0 = y1;
end
end

function dfdx = func(x,y)
dfdx = 1 - y/x;
end