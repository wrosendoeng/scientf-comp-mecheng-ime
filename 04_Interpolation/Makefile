FC = gfortran
FFLAGS = -march=native -mtune=native -O3
FDEBUGFLAGS = -g -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal
IDIR = -I/usr/local/include/fgsl/
LIBS = -lgsl -lfgsl -lopenblas -lm

all: csplinefgsl.exe

csplinefgsl.exe: interpolar_fgsl.o
	$(FC) $(FFLAGS) -o $@ $(IDIR) $^ $(LIBS)

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $@ $(IDIR) $<

debug: interpolar_fgsl.f90
	$(FC) -g $(FDEBUGFLAGS) -o csplinefgsl_dbg.exe $(IDIR) $^ $(LIBS)

clean:
	rm -f *.o *.mod *.exe
