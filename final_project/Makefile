CC = g++
CFLAGS = -march=native -mtune=native -O3
CDEBUGFLAGS = -g 
IDIR = -I/usr/local/include/gsl/
LIBS = -lgsl -lopenblas -lm

all: exec_traj.exe

exec_traj.exe: traj3dof.o
	$(CC) $(CFLAGS) -o $@ $(IDIR) $^ $(LIBS)

traj3dof.o: traj3dof.cpp
	$(CC) -c $(CFLAGS) -o $@ $(IDIR) $<

debug: traj3dof.cpp
	$(CC) -g $(CDEBUGFLAGS) -o traj3_dbg.exe $(IDIR) $^ $(LIBS)

clean:
	rm -f *.o *.mod *.exe