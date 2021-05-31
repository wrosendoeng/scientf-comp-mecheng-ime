#include <iostream>
#include <math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_odeiv2.h>

using namespace std;

struct param_type {
    double cd;
    double d;
    double g;
    double m;
    double rho;
};

int func(double t, const double y[], double f[], void * params)
{
    (void)(t);      /* alerta para evitar parametros inuteis*/   
    struct param_type *my_params_pointer = (struct param_type *)params;

    double cd = my_params_pointer->cd;
    double d = my_params_pointer->d;
    double g = my_params_pointer->g;
    double m = my_params_pointer->m;
    double rho = my_params_pointer->rho;

    /* Definindo as funcoes integradoras */
    f[0] = y[1];                                              // u' = v
    f[1] = M_PI*rho*pow(d,2)*cd*pow(y[1],2)/8.0/m - g;    // v' = aceleracao
    
    return GSL_SUCCESS;
}

int jacobian(double t, const double y[], double * dfdy, double dfdt[], void * params)
{
    (void)(t);      /* alerta para evitar parametros inuteis*/
    struct param_type * my_params_pointer = (struct param_type *)params;

    double cd = my_params_pointer->cd;
    double d = my_params_pointer->d;
    double g = my_params_pointer->g;
    double m = my_params_pointer->m;
    double rho = my_params_pointer->rho;
    
    /* Criando matriz jacobiana */
    gsl_matrix_view dfdy_mat = gsl_matrix_view_array(dfdy, 2, 2); // alocacao dinamica da matriz 2x2
    gsl_matrix * mat = &dfdy_mat.matrix;
    gsl_matrix_set(mat,0,0,0.0);                            // matriz mat(1,1)
    gsl_matrix_set(mat,0,1,1.0);                            // matriz mat(1,2)
    gsl_matrix_set(mat,1,0,0.0);                            // matriz mat(2,1)
    gsl_matrix_set(mat,1,1,-M_PI*rho*pow(d,2)*cd*y[1]/4.0/m);      // matriz mat(2,2)
    
    /* Armazenando o vetor com as aproximacoes de derivadas */
    dfdt[0] = 0.0;
    dfdt[1] = 0.0;

    return GSL_SUCCESS;
}

int main(void)
{    
    struct param_type my_params{0.5, 0.1, 9.81, 1.0, 1.225};

    // armazenar o sistema de equacoes
    gsl_odeiv2_system sys = {func, jacobian, 2, &my_params};

    gsl_odeiv2_driver * d = gsl_odeiv2_driver_alloc_y_new (&sys, gsl_odeiv2_step_rkf45,
                                  1e-8, 1e-8, 0.0);
    int i, s;
    double t = 0.0, t1 = 100.0;
    double y[2] = {1000.0, 0.0};

    for (i = 1; i <= 100; i++)
        {
        double ti = i * t1 / 100.0;
        int status = gsl_odeiv2_driver_apply (d, &t, ti, y);

        if (status != GSL_SUCCESS || y[0] <= 0.0)
            {
            printf ("error, return value=%d\n", status);
            break;
            }
        printf ("%11.5f %.10s %11.5f %.10s %11.5f\n", t," ",y[0]," ",y[1]);
        }

    gsl_odeiv2_driver_free (d);
    return 0;
}

/*
CODIGO IMPORTADO DAS SEGUINTES FONTES:
https://www.gnu.org/software/gsl/doc/html/ode-initval.html#examples
1. Ascher, U.M., Petzold, L.R., Computer Methods for Ordinary Differential 
and Differential-Algebraic Equations, SIAM, Philadelphia, 1998.
2. Hairer, E., Norsett, S. P., Wanner, G., Solving Ordinary Differential 
Equations I: Nonstiff Problems, Springer, Berlin, 1993.
3. Hairer, E., Wanner, G., Solving Ordinary Differential Equations II:
 Stiff and Differential-Algebraic Problems, Springer, Berlin, 1996.
 */
