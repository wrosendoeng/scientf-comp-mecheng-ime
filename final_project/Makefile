# Write this sequence in the terminal:
# ./finalproject initial_conditions.txt environmental_conditions.txt
# prodas.csv V2013_121110-B.csv resultados.txt
FC = gfortran
FFLAGS = -march=native -mtune=native -O3
FDEBUGFLAGS = -g -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal
IDIR = -I/usr/local/include/fgsl/
LIBS = -lgsl -lfgsl -lopenblas -lm

all: finalproject.exe

finalproject.exe: trajectory_4dof_simulator.f90 parameters.o mpmtm_basebleed.o
	$(FC) $(FFLAGS) -o $@ $(IDIR) $^ $(LIBS)

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $@ $(IDIR) $<

debug: trajectory_4dof_simulator.f90 parameters.f90 mpmtm_basebleed.f90
	$(FC) $(FDEBUGFLAGS) -o debug_4dof.exe $(IDIR) $^ $(LIBS)

clean:
	rm -f *.o *.mod *.exe