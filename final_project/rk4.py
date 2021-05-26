# Built-in libraries:
import numpy as np
from numpy import pi, sin, cos
from pandas import DataFrame as df
import matplotlib.pyplot as plt
import matplotlib as mpl
import pandas as pd

# My own libraries and classes:
from mpm_inert import traj4DOF as f1
from mpm_active import traj4DOF as f2

# SETUP CONDITIONS:
v = 878.; # muzzle velocity [m/s] 
twist = 25.; # twist [ca/rev]
ang = 40*pi/180; # elevation angle [degrees]
az = 0.; # azimuthal angle [degrees]
m0 = 20.4043; 
Ix0 = 0.03759; # Axial Inertia at the beginning [kg/m2]
CG0 = 0.407632; # CG from nose at the beginning
m1 = 19.8190; # mass [kg]
Ix1 = 0.03710; # Axial Inertia after BB unit burnout [kg/m2]
CG1 = 0.4022700; # CG from nose [m]
d = 0.112738; # reference diameter [m]

muz1 = np.array([[0,0,0,
                  v*cos(ang)*cos(az),v*sin(ang),v*cos(ang)*sin(az),
                  0,0,0,
                  m1,Ix1,CG1,
                  2*pi*v/(twist*d)]])
muz2 = np.array([[0,0,0,
                  v*cos(ang)*cos(az),v*sin(ang),v*cos(ang)*sin(az),
                  0,0,0,
                  m0,Ix0,CG0,
                  2*pi*v/(twist*d)]])

def numint(f,muz): #numerical integration with 4th order Runge-Kutta
    
	T = np.array([0]);  
	t = 0; # start time
	dt = 0.007; # time step
    
	ran = np.linspace(0,100000,100000,dtype='int')
	FF = muz
	CD = f(t,muz)[1]
    
	for i in ran: # RK4 applied

		k1 = dt*f(t,muz)[0];        
		k2 = dt*f(t+0.5*dt,muz+0.5*k1)[0];
		k3 = dt*f(t+0.5*dt,muz+0.5*k2)[0];
		k4 = dt*f(t+1.0*dt,muz+1.0*k3)[0];
        
		muz = muz + 1/6*(k1+2*(k2+k3)+k4);
        
		t += dt;
        
		maf = f(t,muz)[1]
		FF = np.vstack((FF,muz)); 
        # position, velocity and acceleration
		CD = np.vstack((CD,maf)); 
        # CD0 and Mach number
		T = np.vstack((T,t)); 
        # Time array
        
		#if (FF[i+1,9] <= m1): FF[i+1,9] = m1; 
        # immediate time after propellant burnout condition     
		if (FF[i+1,1] <= 0):  break
    
	cols = ['t','X','Y','Z','Vx [m/s]','Vy [m/s]','Vz [m/s]',
         'Yaw x','Yaw y', 'Yaw z','m [kg]','Ix [kg/m2]',
         'Xcg [m]','spin [rad/s]','Mach','Cd0','Cdb'] 
    
	dflight = df(data=np.hstack((T,FF,CD)),columns=cols)   
        
	return dflight

mpl.rcParams['legend.fontsize']=10
mpl.rcParams['font.family']='sans-serif'
mpl.rcParams['font.sans-serif']=['Arial']
plt.style.use('seaborn-ticks')

dflight1 = numint(f1,muz1)
dflight2 = numint(f2,muz2)

'''dflight1.to_csv('dflight_semBB.csv',index=False,header=True)
dflight2.to_csv('dflight_comBB_dm002.csv',index=False,header=True)'''

# Inert Projectile Positions:
x1 = dflight1['X']
y1 = dflight1['Y']
cd0_1 = dflight1['Cd0']
cdb_1 = dflight1['Cdb']
mach_1 = dflight1['Mach']

# Active Projectile Positions:
x2 = dflight2['X']
y2 = dflight2['Y']
cd0_2 = dflight2['Cd0']
cdb_2 = dflight2['Cdb']
mach_2 = dflight2['Mach']

# PRODAS Data:
aero_coef = pd.read_excel('PRODAS.xlsx')
aero_coef = aero_coef.loc[aero_coef['Mach']<=3]
cd0_prodas = aero_coef['Cx0']
cdb_prodas = aero_coef['Cxb']
mach_prodas = aero_coef['Mach']

fig1, ax1 = plt.subplots()
ax1.plot(x1, y1, '*-', label='Without BB')
ax1.plot(x2, y2, '.-', label='With BB')
ax1.set_ylabel('Height (m)')
ax1.set_xlabel('Downrange (m)')
ax1.legend()
ax1.set_xlim(0,30000)
ax1.set_ylim(0,14000)
ax1.grid(True)
fig1.suptitle('4-DOF MPM Trajectory')
fig1.savefig('fig1-45dm002.png')

fig2, ax2 = plt.subplots()
ax2.plot(mach_prodas, cd0_prodas, '*-', label='PRODAS')
ax2.plot(mach_1, cd0_1, 'x', label='Inert unit')
ax2.plot(mach_2, cd0_2,'.-', label='BB unit')
ax2.set_ylabel('CD0')
ax2.set_xlabel('Mach Number')
ax2.legend()
ax2.grid(True)
fig2.suptitle('Total Drag coefficient at zero-yaw')
fig2.savefig('cd0-40dm002.png')

fig3, ax3 = plt.subplots()
ax3.plot(mach_prodas, cdb_prodas, '*-', label='PRODAS')
ax3.plot(mach_1, cdb_1, 'x', label='Inert unit')
ax3.plot(mach_2, cdb_2,'.-', label='BB unit')
ax3.set_ylabel('CDb')
ax3.set_xlabel('Mach Number')
ax3.legend()
ax3.grid(True)
fig3.suptitle('Base Drag coefficient at zero-yaw')
fig3.savefig('cdb-40dm002.png')

plt.show()
