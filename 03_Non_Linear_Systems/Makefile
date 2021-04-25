FC = gfortran
FFLAGS = -march=native -mtune=native -O3
FDEBUGFLAGS = -g -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal
IDIR = -I/usr/local/include/fgsl/
LIBS = -lgsl -lfgsl -lopenblas -lm

all: ex2dpag206.exe

ex2dpag206.exe: main.o nonlinear.o functions.o
	$(FC) $(FFLAGS) -o $@ $(IDIR) $^ $(LIBS)

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $@ $(IDIR) $<

debug: main.f90 nonlinear.f90 functions.f90
	$(FC) -g $(FDEBUGFLAGS) -o ex2dpag206dbg.exe $(IDIR) $^ $(LIBS)

clean:
	rm -f *.o *.mod *.exe
