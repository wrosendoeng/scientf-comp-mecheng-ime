# -*- coding: utf-8 -*-
"""
Created on Fri Dec 11 01:17:31 2020

@author: Wallace
"""
import numpy as np
from numpy import sin, cos, pi, sqrt, exp
from scipy.linalg import norm
from scipy.interpolate import interp1d
from scipy.integrate import trapz
import pandas as pd

def traj4DOF(time,muz):

    p0 = 1.01325e5; # ambient pressure at sea-level [Pa]
    R = 287.05; # Universal gas constant [J/(Kg.K)]
    ka = 1.4; # Heat capacity ratio
    T0 = 288.15; # ambient temperature at sea-level [K]
    g0 = 9.80665; # gravitational acceleration [m/s^2]
    L = 6.5e-3; # Temperature gradient [K/m]
    omega = 7.292e-05 # Earth angular velocity [rad/s]
    RT = 6.371e06; # Earth radius [m]
    lat = (pi/180)*(-23); # Rio de Janeiro latitude [radianos]
    az = 0; # azimute at muzzle launch
    D = 0.112738; # reference diameter of the projectile [m]
    Db = 0.10690; # boattail diameter [m]
    Ix0 = 0.03759; # Axial Inertia at the beginning [kg/m2]
    Iy0 = 0.47817; # Transverse Inertia at the beginning [kg/m2]
    CG0 = 0.407632; # CG from nose at the beginning
    CG1 = 0.4022700; # CG from nose [m]
    m1 = 19.8190; # projectile mass without BB unit [kg]
    Ix1 = 0.03710; # Axial Inertia after BB unit burnout [kg/m2]
    Iy1 = 0.45798; # Transverse Inertia after BB unit burnout [kg/m2]
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
    aev = np.array([[aex,aey,aez]])
    ae = norm(aev)
        
    # Initial mass:
    m = muz.item(9)
    
    # Initial axial moment of inertia and center of gravity:
    Ix = muz.item(10)
    cg = muz.item(11)
    
    # Spin rate:
    spin = muz.item(12)   
    
    # time at the specific moment:
    t = time
     
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
    mu = Bs*T**(1.5)/(T+Ts)
    # numero de Mach
    Ma = vel/sqrt(ka*R*T)
    # Associated Reynolds number with respect to proj. refer. diameter:
    Rey = rho*D*vel/mu   
    
    # POLYNOMIAL FITTING FOR AERODYNAMIC COEFFICIENTS:
    '''PRODAS.xlsx = source sheet'''    
    aero_coef = pd.read_excel('PRODAS.xlsx');
    
    cd0_function = interp1d(aero_coef['Mach'],aero_coef['Cx0'],kind='cubic')
    cdb_function = interp1d(aero_coef['Mach'],aero_coef['Cxb'],kind='cubic')
    cd2_function = interp1d(aero_coef['Mach'],aero_coef['Cd2'],kind='cubic')
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
    wx = omega*cos(lat)*cos(az)
    wy = omega*sin(lat)
    wz = -omega*cos(lat)*sin(az)
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
    LF = (pi*rho*D**2/8/m)*Cla*vel**2*aev
    
    # MAGNUS FORCE:
    MF = -(pi*rho*D**3*Qm*spin*Cmag_f/8/m)*np.cross(aev,v)
        
    # BASE BLEED FORCE:
    # POLYNOMIAL FITTING FOR HOT MASS FLUX INJECTION:   
    list_cols = ['Time (s)', 'dm/dt (kg/s)', 'Mach', 'Vt (m/s)', 'Tt (K)']
    v2013A0720 = pd.read_csv('V2013_070819_A_21072020.csv',sep=';',header=0,names=list_cols)
    #v2013A1120 = pd.read_csv('V2013_121120-A_27112020.csv',sep=';',header=0,names=list_cols)
    #v2013B1120 = pd.read_csv('V2013_121120-B_27112020.csv',sep=';',header=0,names=list_cols)
    #v2013B0920 = pd.read_csv('V2013_121120-B_27112020.csv',sep=';',header=0,names=list_cols)
    
    v2013A0720_function = interp1d(v2013A0720['Time (s)'],v2013A0720['dm/dt (kg/s)'],kind='cubic')
    #v2013A1120_function = interp1d(v2013A1120['Time (s)'],v2013A1120['dm/dt (kg/s)'],kind='cubic')
    #v2013B1120_function = interp1d(v2013B1120['Time (s)'],v2013B1120['dm/dt (kg/s)'],kind='cubic')
    #v2013B0920_function = interp1d(v2013B0920['Time (s)'],v2013B0920['dm/dt (kg/s)'],kind='cubic')
    
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
