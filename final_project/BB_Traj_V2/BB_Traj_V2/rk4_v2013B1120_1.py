# -*- coding: utf-8 -*-
"""
Created on Fri Dec 11 01:00:55 2020

@author: Wallace

Edited on Monday May 31 20:00:55 2021
@editby: Vitor Pinheiro Pinto
"""
# Built-in libraries:
import numpy as np
from numpy import pi, sin, cos
from pandas import DataFrame as df
from scipy.integrate import trapz
import matplotlib.pyplot as plt
import matplotlib as mpl
import pandas as pd
import time as tbb

import sys, os 
path = os.getcwd()
sys.path.insert(1, path+'\Traj4_Functions')

# My own libraries and classes:
from mpm_inert import traj4DOF as f1
#from mpm_active_v2013B1120 import traj4DOF as f2
# Adicionado um parâmetro, para não dar erro
from mpm_active_v2013B1120_1 import traj4DOF as f2

# SETUP CONDITIONS:
v = 896.; # muzzle velocity [m/s] 
twist = 25.; # twist [ca/rev]
ang = 45*pi/180; # elevation angle [degrees]
az = 0.; # azimuthal angle [degrees]
Ix0 = 0.03759; # Axial Inertia at the beginning [kg/m2]
CG0 = 0.407632; # CG from nose at the beginning
m1 = 19.8190; # mass [kg]
Ix1 = 0.03710; # Axial Inertia after BB unit burnout [kg/m2]
CG1 = 0.4022700; # CG from nose [m]
d = 0.112738; # reference diameter [m]

# NUMERICAL INTEGRATION METHOD TO FIND THE PROPELLENT MASS:
list_cols = ['Time (s)', 'dm/dt (kg/s)', 'Mach', 'Vt (m/s)', 'Tt (K)']
#v2013A0720 = pd.read_csv('V2013_070819_A_21072020.csv',sep=';',header=0,names=list_cols)
#v2013A1120 = pd.read_csv('V2013_121120-A_27112020.csv',sep=';',header=0,names=list_cols)
v2013B1120 = pd.read_csv(path+'\DataBase\V2013_121120-B_27112020.csv',sep=';',header=0,names=list_cols)
#v2013B0920 = pd.read_csv('V2013_140720_B_29092020.csv',sep=';',header=0,names=list_cols)

time = v2013B1120['Time (s)']
dm = v2013B1120['dm/dt (kg/s)']
mprop = trapz(dm,time,dx=0.02) # propellant mass with trapezoidal integration rule

m2 = m1+mprop

muz1 = np.array([[0,0,0,
                  v*cos(ang)*cos(az),v*sin(ang),v*cos(ang)*sin(az),
                  0,0,0,
                  m1,Ix1,CG1,
                  2*pi*v/(twist*d)]])
muz2 = np.array([[0,0,0,
                  v*cos(ang)*cos(az),v*sin(ang),v*cos(ang)*sin(az),
                  0,0,0,
		  m2,Ix0,CG0,
                  2*pi*v/(twist*d)]])

def numint(f,muz, aero_coef, v2013B1120=''): #numerical integration with 4th order Runge-Kutta
    
	T = np.array([0]);  
	t = 0; # start time
	dt = 0.007; # time step
    
	ran = np.linspace(0,100000,100000,dtype='int')
	FF = muz
	CD = f(t,muz, aero_coef, v2013B1120)[1]
    
	progress_scale = 1
	for i in ran: # RK4 applied
		progress = i/1e+3
		if (progress >= progress_scale):
			progress_scale+=1
			print(f'{progress} %')
		
		t0 = tbb.time()
		k1 = dt*f(t,muz, aero_coef, v2013B1120)[0]
		k2 = dt*f(t+0.5*dt,muz+0.5*k1, aero_coef, v2013B1120)[0]
		k3 = dt*f(t+0.5*dt,muz+0.5*k2, aero_coef, v2013B1120)[0]
		k4 = dt*f(t+1.0*dt,muz+1.0*k3, aero_coef, v2013B1120)[0]
        

		muz = muz + 1/6*(k1+2*(k2+k3)+k4)
        
		t += dt
        
		maf = f(t,muz, aero_coef, v2013B1120)[1]

		#t0 = tbb.time()
		FF = np.vstack((FF,muz)) 
        # position, velocity and acceleration
		CD = np.vstack((CD,maf))
        # CD0 and Mach number
		T = np.vstack((T,t))
        # Time array
        
		# if (FF[i+1,9] <= m1): FF[i+1,9] = m1; 
        # immediate time after propellant burnout condition     
		if (FF[i+1,1] <= 0): 
			break
    
	cols = ['t','X','Y','Z',
	'Vx [m/s]','Vy [m/s]','Vz [m/s]','Yaw x',
	'Yaw y','Yaw z','m [kg]','Ix [kg/m2]',
         'Xcg [m]','spin [rad/s]','Mach','Cd0','Cdb'] 
    
	dflight = df(data=np.hstack((T,FF,CD)),columns=cols)   
        
	return dflight

mpl.rcParams['legend.fontsize']=10
mpl.rcParams['font.family']='sans-serif'
mpl.rcParams['font.sans-serif']=['Arial']
plt.style.use('seaborn-ticks')

aero_coef = pd.read_excel(path+'\DataBase\PRODAS-MODIFICADO.xlsx', engine='openpyxl')

t0 = tbb.time()
print('First! Fire!', 20*'-')
dflight1 = numint(f1, muz1, aero_coef)
print(f'Down in {tbb.time() - t0} s !', 30*'-')

t1 = tbb.time()
print('Second! Fire!', 20*'-')
dflight2 = numint(f2, muz2, aero_coef, v2013B1120)
print(f'Down in {tbb.time() - t1} s !', 30*'-')

print(f'\nThe simulation took {tbb.time() - t0} s to end.')

dflight1.to_csv('dflight_semBB.csv',index=False,header=True)
dflight2.to_csv('dflight_comBB_v2013B1120.csv',index=False,header=True)

# Inert Projectile Positions:
x1 = dflight1['X']
y1 = dflight1['Y']
cd0_1 = dflight1['Cd0']
mach_1 = dflight1['Mach']

# Active Projectile Positions:
x2 = dflight2['X']
y2 = dflight2['Y']
cd0_2 = dflight2['Cd0']
mach_2 = dflight2['Mach']

# PRODAS Data:
aero_coef = aero_coef.loc[aero_coef['Mach']<=3]
cd0_prodas = aero_coef['Cx0']
mach_prodas = aero_coef['Mach']

fig1, ax1 = plt.subplots()
ax1.plot(x1, y1, '*-', label='Without BB')
ax1.plot(x2, y2, '.-', label='With BB')
ax1.set_ylabel('Height (m)')
ax1.set_xlabel('Downrange (m)')
ax1.legend()
ax1.set_xlim(0,30000)
ax1.set_ylim(0,14000)
fig1.suptitle('4-DOF MPM Trajectory - V2013_121120-B_27112020')
fig1.savefig('fig1-V2013_121120-B_27112020.png')

fig2, ax2 = plt.subplots()
ax2.plot(mach_prodas, cd0_prodas, '*-', label='PRODAS')
ax2.plot(mach_1, cd0_1, 'x', label='Inert unit')
ax2.plot(mach_2, cd0_2,'.-', label='BB unit')
ax2.set_ylabel('CD0')
ax2.set_xlabel('Mach Number')
ax2.legend()
fig2.suptitle('Drag coefficient at zero-yaw')
fig2.savefig('fig2-V2013_121120-B_27112020.png')

plt.show()
