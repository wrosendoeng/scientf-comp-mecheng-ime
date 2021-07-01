import numpy as np
from numpy import sin, cos, pi, sqrt, exp
from scipy.linalg import norm
from scipy.interpolate import interp1d
import pandas as pd

def traj4DOF(time,muz, aero_coef, v2013B1120):
    
    p0 = 1.01325e5; # ambient pressure at sea-level [Pa]  
    R = 287.05; # Universal gas constant [J/(Kg.K)]    
    ka = 1.4; # Heat capacity ratio
    T0 = 288.15; # K
    g0 = 9.80665; # gravitational acceleration [m/s^2]    
    L = 6.5e-3; # Temperature gradient [K/m]    
    omega = 7.292e-05 # Earth angular velocity [rad/s]    
    RT = 6.371e06;  # Earth radius [m]    
    lat = (pi/180)*(-23); # Rio de Janeiro latitude [radians]    
    D = 0.112738; # reference diameter of the projectile [m]    
    m0 = 20.40430; # projectile mass with BB unit [kg]   
    Ix0 = 0.03759; # Axial Inertia at the beginning [kg/m2]    
    Iy0 = 0.47817; # Transverse Inertia at the beginning [kg/m2]    
    CG0 = 0.407632; # CG from nose at the beginning    
    m1 = 19.8190; # projectile mass without BB unit [kg]    
    Ix1 = 0.03710; # Axial Inertia after BB unit burnout [kg/m2]    
    Iy1 = 0.45798; # Transverse Inertia after BB unit burnout [kg/m2]    
    CG1 = 0.4022700; # CG from nose [m]
    Bs = 1.458e-6 # kg/(m.s.K^1/2)
    Ts = 110.4 # Sutherland's temperature constant [K]
            
    # Fitting factor:    
    Qm = 1.2; # magnus force factor    
    Qd = 1.2; # drag force factor   
            
    # Position wrt the ground:    
    X = muz.item(0)    
    H = muz.item(1)    
    Z = muz.item(2)
            
    # Velocity wrt the ground:    
    vx = muz.item(3)    
    vy = muz.item(4)    
    vz = muz.item(5)    
    v = np.array([[vx,vy,vz]])    
    vel = norm(v)    
        
    # Yaw of repose:    
    aex = muz.item(6)    
    aey = muz.item(7)    
    aez = muz.item(8)    
    yaw = np.array([[aex,aey,aez]])   
    ae = norm(yaw)
               
    # Initial mass:    
    m = muz.item(9)    
       
    # Initial axial moment of inertia and center of gravity:    
    Ix = muz.item(10)
            
    # Spin rate:
    spin = muz.item(12)       
       
    # Gravity acceleration in spherical approximation:     
    g = g0*(1 - 0.0026*cos(2*lat))        
       
    if (H <= 1.1e04):     
        T = T0 - L*H;      
        p = p0*(1 - L*H/T0)**(g0/L/R);
      
    else:
        T = 216.65;
        p = (22620.115099247665)*exp(-g0/R/T*(H-11000));
           
    # Free air stream density:
    rho = p/R/T 
    # Sutherland's law of Dynamic Viscosity:
    mu = Bs/(T+Ts)*T**(1.5)
    # numero de Mach
    Ma = vel/sqrt(ka*R*T)
    # Associated Reynolds number with respect to proj. refer. diameter:
    Rey = rho*D*vel/mu    
    
    # POLYNOMIAL FITTING FOR AERODYNAMIC COEFFICIENTS:
    '''PRODAS.xlsx = source sheet'''    
    #aero_coef = pd.read_excel('PRODAS.xlsx');
    #aero_coef = pd.read_excel('PRODAS-MODIFICADO.xlsx', engine='openpyxl');
    cd0_function = interp1d(aero_coef['Mach'],aero_coef['Cx0'],kind='cubic')
    cdb_function = interp1d(aero_coef['Mach'],aero_coef['Cxb'],kind='cubic')
    #cd2_function = interp1d(aero_coef['Mach'],aero_coef['Cd2'],kind='cubic')
    cd2_function = interp1d(aero_coef['Mach'],aero_coef['Cx2'],kind='cubic')
    cla_function = interp1d(aero_coef['Mach'],aero_coef['Cla'],kind='cubic')
    cmag_function = interp1d(aero_coef['Mach'],aero_coef['Cmag_f'],kind='cubic')
    cma_function = interp1d(aero_coef['Mach'],aero_coef['Cma'],kind='cubic')
    cspin_function = interp1d(aero_coef['Mach'],aero_coef['Cspin'],kind='cubic')
    
    Cd0 = cd0_function(Ma)
    Cdb = cdb_function(Ma)
    Cd2 = cd2_function(Ma)
    Cla = cla_function(Ma)
    Cmag_f = cmag_function(Ma)
    Cma = cma_function(Ma)
    Cspin = cspin_function(Ma)
               
    # CORIOLIS EFFECT: 2*omega*[Cx,Cy,Cz]
    wx = omega*cos(lat)
    wy = omega*sin(lat)
    wz = 0
    w = np.array([[wx,wy,wz]])
    CF = -2*np.cross(w,v)    
    
    # DRAG FORCE:
    DF = -(pi*rho*D**2/8/m)*(Cd0+Cd2*(Qd*ae)**2)*vel*v
    
    # GRAVITATIONAL FORCE DUE TO SPHERIC APPROXIMATION:
    gx = -g*X/RT   
    gy = -g*(1 - 2*H/RT)    
    gz = -g*Z/RT 
    GF = np.array([[gx,gy,gz]])
        
    # LIFT FORCE:
    LF = (pi*rho*D**2/8/m)*Cla*vel**2*yaw
    
    # MAGNUS FORCE:
    MF = -(pi*rho*D**3*Qm*spin*Cmag_f/8/m)*np.cross(yaw,v)
                        
    # NUMERICAL INTEGRATION: 
    # acceleration vector:
    acc = CF+DF+GF+LF+MF
    # yaw of repose:
    yor = -8*Ix*spin*np.cross(v,acc)/(pi*rho*D**3*Cma*vel**4)
    # mass flow vector:
    mrt = np.array([[0,(Ix0-Ix1)*(m-m0)/(m0-m1),(CG0-CG1)*(m-m0)/(m0-m1)]])
    # spin rate vector:
    spr = np.array([[pi*rho*D**4*spin*vel*Cspin/(8*Ix)]]) 
    FF = np.hstack((v,acc,yor,mrt,spr))
    CDMA = np.array([[Ma,Cd0,Cdb]])
    
    return FF, CDMA
