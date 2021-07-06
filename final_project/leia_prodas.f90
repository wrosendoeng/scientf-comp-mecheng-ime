! gfortran -march=native -mtune=native -O3 -o testando leia_prodas.f90 
! -I/usr/local/include/fgsl/ -lgsl -lfgsl -lopenblas -lm
function prodas_interp(x,y,n) result(y_interp)
    implicit none
    
    integer(4) :: status
    integer(fgsl_size_t) :: n
    real(8) :: x(:), y(:), x_interp, y_interp
    type(fgsl_interp_accel) :: acc
    type(fgsl_spline) :: cspline

    allocate(x(n), y(n))

    acc = fgsl_interp_accel_alloc()
    cspline = fgsl_spline_alloc(fgsl_interp_cspline, n)
    status = fgsl_spline_init(cspline, x, y)
    y_interp = fgsl_spline_eval(cspline,x_interp,acc)
    
    call fgsl_spline_free(cspline)
    call fgsl_interp_accel_free(acc)

    deallocate(x,y)
    
end function leia_prodas