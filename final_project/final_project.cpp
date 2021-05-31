// C++ program to calculate 4-DOF trajectory of
// an spin-stabilized axissymetric projectile
// Final project of Scientific Computation Class
// Mechanical Engineering Master's Course
// Student: Wallace Ramos Rosendo da Silva
// Professor: Mj Achilles Arantes Bassi

#include <iostream>
#include <fstream>
#include <vector>
#include <math.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_multifit.h>
#include <gsl/gsl_odeiv2.h>

using namespace std;
// #define pi 3.1416

/*
void read_data()
{
    ifstream data;
    data.open('env_data.txt');
    cout << 
    data.close();
}
*/

Vector::Vector(int s)
    :elem{new double[s]}, sz{s}
{
}

double read_and_sum(int s)
// read s integers cin and return their sum; s is assumed to be positive
{
    Vector v(s);
    for (int i=0; i!=v.size(); ++i)        
        cin >> v[i];           // read into elements

    double sum = 0;
    for (int i=0; i!=v.size(); ++i)
        sum += v[i];           // take the sum of the elements 
    return sum;
}

/*
class ambient{
    private:
        float p0, R, ka, g0, L, omega, RT, lat, az, bs, suth;
}; */

/*
class geometry{
    private:
        float d, db, ix0, iy0, cg0, cg1, m1, ix1, iy1, qm, qd;
}; */

/*
float env_parameters(void){

    ambient disparo;

//  Gravity acceleration in spherical approximation: 
    g = g0*(1 - 0.0026*cos(2*lat));
        
    if (y <= 1.1e04)
    {
        T = T0 - L*y; 
        p = p0*(1 - L*y/T0)**(g0/L/R);
    }
    else
    {
        T = 216.65;
        p = (22620.115099247665)*exp(-g0/R/T*(H-11000));
    }
        
//  Free air stream density:
    rho = p/R/T; 
//  Sutherland's law of Dynamic Viscosity:
    mu = bs*T**(1.5)/(T+suth);
//  numero de Mach
    Ma = vel/sqrt(ka*R*T);
//  Associated Reynolds number with respect to proj. refer. diameter:
    Rey = rho*D*vel/mu;

}
*/

/*
float aerocoefs(void){

//  POLYNOMIAL FITTING FOR AERODYNAMIC COEFFICIENTS:
    '''PRODAS.xlsx = source sheet'''    
    aero_coef = pd.read_excel('PRODAS.xlsx');
    
    cd0_function = interp1d(aero_coef['Mach'],aero_coef['Cx0'],kind='cubic');
    cdb_function = interp1d(aero_coef['Mach'],aero_coef['Cxb'],kind='cubic');
    cd2_function = interp1d(aero_coef['Mach'],aero_coef['Cd2'],kind='cubic');
    cla_function = interp1d(aero_coef['Mach'],aero_coef['Cla'],kind='cubic');
    cmag_function = interp1d(aero_coef['Mach'],aero_coef['Cmag_f'],kind='cubic');
    cma_function = interp1d(aero_coef['Mach'],aero_coef['Cma'],kind='cubic');
    cspin_function = interp1d(aero_coef['Mach'],aero_coef['Cspin'],kind='cubic');
    
    Cd0 = cd0_function(mach);
    Cdb = cdb_function(mach);
    Cd2 = cd2_function(mach);
    Cla = cla_function(mach);
    Cmag_f = cmag_function(mach);
    Cma = cma_function(mach);
    Cspin = cspin_function(mach);
}

float traj4dof(int n, float* vetor(n), float time){

    int i, n;
    float vsum, vel, aesum, ae;
    vector<float> v(12);
    vector<float> result();
    
//  Position wrt the ground:
    x = v[0];
    y = v[1];
    z = v[2];
    
//  Velocity wrt the ground:
    vx = v[3];
    vy = v[4];
    vz = v[5];
    for (i = 3; i < 6; i++){
        vsum += v[i]*v[i];
    }
    vel = sqrt(vsum);
    
//  Yaw of repose:
    aex = v[6];
    aey = v[7];
    aez = v[8];
    for (i = 6; i < 9; i++){
        aesum += v[i]*v[i];
    }
    ae = sqrt(aesum);

//  Initial mass, axial moment of inertia and center of gravity::
    m = v[9];
    ix = v[10];
    cg = v[11];
    
//  Spin rate:
    spin = v[12];


   

               
//  CORIOLIS EFFECT: 2*omega*[Cx,Cy,Cz]
    wx = omega*cos(lat)*cos(az)
    wy = omega*sin(lat)
    wz = -omega*cos(lat)*sin(az)
    w = np.array([[wx,wy,wz]])
    CF = -2*np.cross(w,v)    
    
//  DRAG FORCE:
    DF = -(pi*rho*D**2/8/m)*(Cd0+Cd2*(Qd*ae)**2)*vel*v
    
//  GRAVITATIONAL FORCE DUE TO SPHERIC APPROXIMATION:
    gx = -g*x/RT
    gy = -g*(1 - 2*y/RT)
    gz = -g*Z/RT
    GF[3] = {gx,gy,gz}; /* NAO ESQUECER DE DECLARAR COMO VARIAVEL */
    
//  LIFT FORCE:
    /* LF = (pi*rho*D**2/8/m)*Cla*vel**2*aev; /* NAO ESQUECER DE DECLARAR COMO VARIAVEL */

/*   
// MAGNUS FORCE:
    MF = -(pi*rho*D**3*Qm*spin*Cmag_f/8/m)*np.cross(aev,v)
        
    # BASE BLEED FORCE:
    # POLYNOMIAL FITTING FOR HOT MASS FLUX INJECTION:   
    list_cols = ['Time (s)', 'dm/dt (kg/s)', 'Mach', 'Vt (m/s)', 'Tt (K)']
    v2013A0720 = pd.read_csv('V2013_070819_A_21072020.csv',sep=';',header=0,names=list_cols)
    #v2013A1120 = pd.read_csv('V2013_121120-A_27112020.csv',sep=';',header=0,names=list_cols)
    #v2013B1120 = pd.read_csv('V2013_121120-B_27112020.csv',sep=';',header=0,names=list_cols)
    #v2013B0920 = pd.read_csv('V2013_121120-B_27112020.csv',sep=';',header=0,names=list_cols)
    
    v2013A0720_function = interp1d(v2013A0720['Time (s)'],v2013A0720['dm/dt (kg/s)'],kind='cubic');
    #v2013A1120_function = interp1d(v2013A1120['Time (s)'],v2013A1120['dm/dt (kg/s)'],kind='cubic');
    #v2013B1120_function = interp1d(v2013B1120['Time (s)'],v2013B1120['dm/dt (kg/s)'],kind='cubic');
    #v2013B0920_function = interp1d(v2013B0920['Time (s)'],v2013B0920['dm/dt (kg/s)'],kind='cubic');
    
    time_list = v2013A0720['Time (s)']
    dm_list = v2013A0720['dm/dt (kg/s)']
    mprop = trapz(dm_list,time_list,dx=0.02) # propellant mass with trapezoidal integration rule
    
    m0 = m1 + mprop
    
    if t <= v2013A0720['Time (s)'].iloc[-1]:
        dm = v2013A0720_function(t)
    else:
        dm = 0
        
    def injection_factor(t,rho,vel,p,spin,Db,dm):
            
        I0 = 0.005;
        'dm = 0.02 # mass flow rate = 20 grams per second'
        I = 4*dm/(pi*rho*vel*Db**2)
        
        if I < I0: 
            inj = I/I0;
        else:
            inj = 1.0;
        
        return inj
    
    inj = injection_factor(t,rho,vel,p,spin,Db,dm) 
    
    BBF = +pi*rho*(D*vel)**2*Cdb*inj*(v*cos(ae)/vel+aev)/(8*m)
            
    # NUMERICAL INTEGRATION:
    # acceleration vector:
    acc = CF+DF+GF+LF+MF+BBF
    # yaw of repose:
    yor = -8*Ix*spin*np.cross(v,acc)/(pi*rho*D**3*Cma*vel**4)
    # mass flow vector:
    mrt = np.array([[-dm,(Ix0-Ix1)*(m-m0)/(m0-m1),(CG0-CG1)*(m-m0)/(m0-m1)]])
    # spin rate vector:
    spr = np.array([[pi*rho*(D**4)*spin*vel*Cspin/(8*Ix)]]) 
    FF = np.hstack((v,acc,yor,mrt,spr))  
    CDMA = np.array([[Ma,Cd0]])
    
    return FF, CDMA
}

double rk4(float f, float muzzle, float dt){

    int i, n;
    double k1, k2, k3, k4;

	for (i = 0; i = 100; i++){                      // 4th order Runge-Kutta
        k1 = k1 + dt*f(t,muz)
        k2 = k2 + dt*f(t+0.5*dt,muz+0.5*k1)[0];
		k3 = k3 + dt*f(t+0.5*dt,muz+0.5*k2)[0];
		k4 = k4 + dt*f(t+1.0*dt,muz+1.0*k3)[0];     
    }        

    muzzle = muzzle + 1/6*(k1+2*(k2+k3)+k4);        // Muzzle conditions
        
		t += dt;
        
		maf = f(t,muz)[1]
		FF = np.vstack((FF,muz)); 
                        // position, velocity and acceleration
		CD = np.vstack((CD,maf)); 
                        // CD0 and Mach number
		T = np.vstack((T,t)); 
                        // Time array
        
		#if (FF[i+1,9] <= m1): FF[i+1,9] = m1; 
                        // immediate time after propellant burnout condition     
		if (FF[i+1,1] <= 0):  break

    return 0;
}

int main(){
    /* initialize variables */
/*
    v = 878.;           // muzzle velocity [m/s] 
    twist = 25.;        // twist [ca/rev]
    ang = 40*pi/180;    // elevation angle [degrees]
    az = 0.;            // azimuthal angle [degrees]
    m0 = 20.4043;       // initial mass [kg]
    Ix0 = 0.03759;      // Axial Inertia at the beginning [kg/m2]
    CG0 = 0.407632;     // CG from nose at the beginning
    m1 = 19.8190;       // mass [kg]
    Ix1 = 0.03710;      // Axial Inertia after BB unit burnout [kg/m2]
    CG1 = 0.4022700;    // CG from nose [m]
    d = 0.112738;       // reference diameter [m]
    return 0;
} */