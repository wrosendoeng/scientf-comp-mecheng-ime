FC = gfortran
FFLAGS = -march=native -mtune=native -O3
FDEBUGFLAGS = -g -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal
IDIR = -I/usr/local/include/fgsl/
LIBS = -lgsl -lfgsl -lopenblas -lm

all: chap04nonlinear.exe

chap04nonlinear.exe: functions.o jacobian_matrices.o nonlinear.o direct_resolution_methods.o iterative_resolution_methods.o main.o
	$(FC) $(FFLAGS) -o $@ $(IDIR) $^ $(LIBS)

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $@ $(IDIR) $<

debug: main.f90 nonlinear.f90 functions.f90 jacobian_matrices.f90 direct_resolution_methods.f90 iterative_resolution_methods.f90
	$(FC) -g $(FDEBUGFLAGS) -o chap04nonlineardbg.exe $(IDIR) $^ $(LIBS)

clean:
	rm -f *.o *.mod *.exe
