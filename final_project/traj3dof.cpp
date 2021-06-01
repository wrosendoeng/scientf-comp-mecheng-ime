#include <iostream>
#include <fstream>
#include <math.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_odeiv2.h>

using namespace std;

struct param_type {
    double airdens;         // air density [kg/m3]
    double axinertia_init;  // initial axial inertia [kg/m2]
    double axinertia_fin;   // final axial inertia [kg/m2]
    double azim;            // azimuth [radians]
    double boatdim;         // boattail diameter [m]
    double bsuth;           // sutherland constant for viscosity [kg/(m.s.K^1/2)]
    double centgrav0;       // initial center of gravity [m]
    double centgrav1;       // final center of gravity [m]
    double dragfactor;      // drag force factor
    double magfactor;       // magnus force factor
    double gravity;         // acceleration due to gravity [m/s2]
    double heatcap;         // heat ratio capacity
    double lat;             // latitude of launch [radians]
    double massinit;        // initial projectile mass [kg] 
    double massfin;         // final projectile mass [kg]
    double omega;           // earth angular velocity [rad/s]
    double pinit;           // ambient pressure at sea-level [N/m2]
    double radearth;        // earth radius
    double refdim;          // reference of diameter [mm]
    double univgasconst;    // universal gas constant [J/(kg.K)]
    double tempgrad;        // temperature gradient [K/m]
    double tempinit;        // ambient temperature at sea-level [K]
    double tsuth;           // sutherland's temperature constant [K] 
};

int func(double t, const double y[], double f[], void * params)
{
    (void)(t);      /* alerta para evitar parametros inuteis*/
    double airdens, mu, p, T;   
    struct param_type *my_params_pointer = (struct param_type *)params;

    double airdens = my_params_pointer->airdens;                // free air stream density
    double axinertia_init = my_params_pointer-> axinertia_init; // initial axial moment of inertia
    double axinertia_fin = my_params_pointer-> axinertia_fin;   // final axial moment of inertia
    double azim = my_params_pointer -> azim;                    // azimuth [radians]
    double boatdim = my_params_pointer -> boatdim;              // boattail diameter [m]
    double bsuth = my_params_pointer ->bsuth;                   // sutherland constant for viscosity [kg/(m.s.K^1/2)]
    double centgrav0 = my_params_pointer -> centgrav0;          // initial center of gravity [m]
    double centgrav1 = my_params_pointer -> centgrav1;          // final center of gravity [m]
    double dragfactor = my_params_pointer -> dragfactor;        // drag force factor
    double magfactor = my_params_pointer -> magfactor;          // magnus force factor
    double gravity = my_params_pointer -> gravity;              // acceleration due to gravity [m/s2]
    double heatcap = my_params_pointer -> heatcap;              // heat ratio capacity
    double lat = my_params_pointer -> lat;                      // latitude of launch [radians]
    double massinit = my_params_pointer -> massinit;            // initial projectile mass [kg] 
    double massfin = my_params_pointer -> massfin;              // final projectile mass [kg]
    double omega = my_params_pointer -> omega;                  // earth angular velocity [rad/s]
    double pinit = my_params_pointer -> pinit;                  // ambient pressure at sea-level [N/m2]
    double radearth = my_params_pointer -> radearth;            // earth radius
    double refdim = my_params_pointer -> refdim;                // reference of diameter [mm]
    double univgasconst = my_params_pointer -> univgasconst;    // universal gas constant [J/(kg.K)]
    double tempgrad = my_params_pointer -> tempgrad;            // temperature gradient [K/m]
    double tempinit = my_params_pointer -> tempinit;            // ambient temperature at sea-level [K]
    double tsuth = my_params_pointer -> tsuth;                  // sutherland's temperature constant [K] 

    double vel = sqrt(pow(y[3],2)+pow(y[4],2)+pow(y[5],2));
    double g = gravity*(1-0.0026*cos(lat));

    if (y[1] <= 1.1e4)
    {
        T = tempinit - tempgrad*y[1];      
        p = pinit*pow(1 - tempgrad*y[1]/tempinit,(g0/L/R));
    } 
    else
    {
        T = 216.65;
        p = 2.262e4*exp(-gravity/univgasconst/T*(y[1]-1.1e4)); 
    }
    
    airdens = p/univgasconst/T;             // Free air stream density:
    mu = bsuth/(T+tsuth)*pow(T,1.5);        // Sutherland's law of dynamic viscosity
    Ma = vel/sqrt(heatcap*univgasconst*T);  // Mach Number
    Rey = airdens*refdim*vel/mu;            // Reynolds Number referred to diameter

    // YAW MOTION:
    double yaw[0] = y[6];
    double yaw[1] = y[7];
    double yaw[2] = y[8];
    double ae = sqrt(pow(yaw[0],2)+pow(yaw[1],2)+pow(yaw[2],2));

    // NON-CONSTANT VARIABLES:
    double m = y[9];        // mass
    double ix = y[10];      // axial inertia
    double cg = y[11];      // center of gravity
    double spin = y[12];    // spin rate

    // CORIOLIS EFFECT:
    double cf[0] = -y[4]*cos(lat)*cos(az) - y[5]*sin(lat);
    double cf[1] = -y[3]*cos(lat)*sin(az) + y[5]*cos(lat)*cos(az);
    double cf[2] = y[3]*sin(lat) - y[4]*cos(lat)*cos(lat);

    // GRAVITY FORCE:
    double gf[0] = -g*y[0]/radearth;
    double gf[1] = -g*(1-2*y[1]/radearth);
    double gf[2] = -g*y[2]/radearth;

    // DRAG FORCE: 
    double df[0] = -M_PI*airdens*pow(d,2)*cd*y[3]*vel/8.0/m;
    double df[1] = -M_PI*airdens*pow(d,2)*cd*y[4]*vel/8.0/m;
    double df[2] = -M_PI*airdens*pow(d,2)*cd*y[5]*vel/8.0/m;

    // LIFT FORCE: 
    double lf[0] = -M_PI*airdens*pow(refdim,2)*Cla*vel*yaw[0]/8.0/m;
    double lf[1] = -M_PI*airdens*pow(refdim,2)*Cla*vel*yaw[1]/8.0/m;
    double lf[2] = -M_PI*airdens*pow(refdim,2)*Cla*vel*yaw[2]/8.0/m;
    
    // MAGNUS FORCE:
    double mf[0] = -M_PI*airdens*pow(refdim,2)*Cmag/8.0/m*(y[5]*yaw[1]-y[4]*yaw[2]);
    double mf[1] = -M_PI*airdens*pow(refdim,2)*Cmag/8.0/m*(y[3]*yaw[2]-y[5]*yaw[0]);
    double mf[2] = -M_PI*airdens*pow(refdim,2)*Cmag/8.0/m*(y[2]*yaw[0]-y[1]*yaw[1]);

    // YAW OF REPOSE COEFFICIENT:
    double yor = -8.0*ix*spin/(M_PI*airdens*pow(refdim,3)*Cma*pow(vel,4));

    /* Definindo as funcoes integradoras */
    f[0] = y[3];                                                    // ux' = vx
    f[1] = y[4];                                                    // uy' = vy
    f[2] = y[5];                                                    // uz' = vz
    f[3] = cf[0] + gf[0] + df[0] + lf[0] + mf[0];                   // vx' = ax
    f[4] = cf[1] + gf[1] + df[1] + lf[1] + mf[1];                   // vy' = ay
    f[5] = cf[2] + gf[2] + df[2] + lf[2] + mf[2];                   // vz' = az
    f[6] = yor*(f[1]*f[5]-f[2]*f[4]);
    f[7] = yor*(f[3]*f[2]-f[0]*f[5]);
    f[8] = yor*(f[0]*f[4]-f[1]*f[3]);
    f[9] = 0.0;
    f[10] = (ix0-ix1)*(m-massinit)/(massinit-massfin);
    f[11] = (cg0-cg1)*(m-massinit)/(massinit-massfin);
    f[12] = M_PI*airdens*pow(refdim,4)*spin*vel*Cspin/(8.0*ix);

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
    double airdens = my_params_pointer->airdens;
    
    double vel = sqrt(pow(y[3],2)+pow(y[4],2)+pow(y[5],2));
    
    /* Criando matriz jacobiana */
    gsl_matrix_view dfdy_mat = gsl_matrix_view_array(dfdy, 12, 12); // alocacao dinamica da matriz 2x2
    gsl_matrix * mat = &dfdy_mat.matrix;
    
    /* Linha 1*/
    gsl_matrix_set(mat,0,0,0.0);                                    // matriz mat(1,1)
    gsl_matrix_set(mat,0,1,0.0);                                    // matriz mat(1,2)
    gsl_matrix_set(mat,0,2,0.0);                                    // matriz mat(1,3)
    gsl_matrix_set(mat,0,3,1.0);                                    // matriz mat(1,4)
    gsl_matrix_set(mat,0,4,0.0);                                    // matriz mat(1,5)
    gsl_matrix_set(mat,0,5,0.0);                                    // matriz mat(1,6)
    
    /* Linha 2*/
    gsl_matrix_set(mat,1,0,0.0);                                    // matriz mat(2,1)
    gsl_matrix_set(mat,1,1,0.0);                                    // matriz mat(2,2)
    gsl_matrix_set(mat,1,2,0.0);                                    // matriz mat(2,3)
    gsl_matrix_set(mat,1,3,0.0);                                    // matriz mat(2,4)
    gsl_matrix_set(mat,1,4,1.0);                                    // matriz mat(2,5)
    gsl_matrix_set(mat,1,5,0.0);                                    // matriz mat(2,6)

    /* Linha 3*/
    gsl_matrix_set(mat,2,0,0.0);                                    // matriz mat(3,1)
    gsl_matrix_set(mat,2,1,0.0);                                    // matriz mat(3,2)
    gsl_matrix_set(mat,2,2,0.0);                                    // matriz mat(3,3)
    gsl_matrix_set(mat,2,3,0.0);                                    // matriz mat(3,4)
    gsl_matrix_set(mat,2,4,0.0);                                    // matriz mat(3,5)
    gsl_matrix_set(mat,2,5,1.0);                                    // matriz mat(3,6)
    
    /* Linha 4*/
    gsl_matrix_set(mat,3,0,0.0);                                    // matriz mat(3,1)
    gsl_matrix_set(mat,3,1,0.0);                                    // matriz mat(3,2)
    gsl_matrix_set(mat,3,2,0.0);                                    // matriz mat(3,3)
    gsl_matrix_set(mat,3,3,-M_PI*airdens*pow(d,2)*cd*vel/8.0/m);        // matriz mat(3,4)
    gsl_matrix_set(mat,3,4,0.0);                                    // matriz mat(3,5)
    gsl_matrix_set(mat,3,5,1.0);                                    // matriz mat(3,6)
    
    /* Linha 5*/
    gsl_matrix_set(mat,4,0,0.0);                                    // matriz mat(4,1)
    gsl_matrix_set(mat,4,1,0.0);                                    // matriz mat(4,2)
    gsl_matrix_set(mat,4,2,0.0);                                    // matriz mat(4,3)
    gsl_matrix_set(mat,4,3,0.0);                                    // matriz mat(4,4)
    gsl_matrix_set(mat,4,4,-M_PI*airdens*pow(d,2)*cd*vel/8.0/m);        // matriz mat(4,5)
    gsl_matrix_set(mat,4,5,1.0);                                    // matriz mat(4,6)
    
    /* Linha 6*/
    gsl_matrix_set(mat,5,0,0.0);                                    // matriz mat(5,1)
    gsl_matrix_set(mat,5,1,0.0);                                    // matriz mat(5,2)
    gsl_matrix_set(mat,5,2,0.0);                                    // matriz mat(5,3)
    gsl_matrix_set(mat,5,3,0.0);                                    // matriz mat(5,4)
    gsl_matrix_set(mat,5,4,0.0);                                    // matriz mat(5,5)
    gsl_matrix_set(mat,5,5,-M_PI*airdens*pow(d,2)*cd*vel/8.0/m);        // matriz mat(5,6) 
    
    /* Armazenando o vetor com as aproximacoes de derivadas */
    dfdt[0] = 0.0;
    dfdt[1] = 0.0;
    dfdt[2] = 0.0;
    dfdt[3] = 0.0;
    dfdt[4] = 0.0;
    dfdt[5] = 0.0;

    return GSL_SUCCESS;
}

int main(void)
{    
    struct param_type my_params{0.5, 0.1, 9.81, 1.0, 1.225};

    // armazenar o sistema de equacoes
    gsl_odeiv2_system sys = {func, jacobian, 12, &my_params};

    gsl_odeiv2_driver * d = gsl_odeiv2_driver_alloc_y_new (&sys, gsl_odeiv2_step_rk4,
                                  1e-8, 1e-8, 0.0);
    int i, s;
    double t = 0.0, t1 = 100.0;
    double y[12] = {0.0,0.0,0.0,900.0,900.0,50.0};

    for (i = 1; i <= 100; i++)
        {
        double ti = i * t1 / 100.0;
        int status = gsl_odeiv2_driver_apply (d, &t, ti, y);

        if (status != GSL_SUCCESS || y[1] < 0.0)
            {
            printf ("error, return value=%d\n", status);
            break;
            }
        printf ("%11.5f %.10s %11.5f %.10s %11.5f %.10s %11.5f %.10s %11.5f %.10s %11.5f %.10s %11.5f\n", t," ",y[0]," ",y[1]," ",y[2]," ",y[4]," ",y[4]," ",y[5]);
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