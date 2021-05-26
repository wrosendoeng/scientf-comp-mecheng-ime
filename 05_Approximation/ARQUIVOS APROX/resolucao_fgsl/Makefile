FC = gfortran
FFLAGS = -march=native -mtune=native -O3
FDEBUGFLAGS = -g -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal
IDIR = -I/usr/local/include/fgsl/
LIBS = -lgsl -lfgsl -lopenblas -lm

all: fitting_fgsl.exe

fitting_fgsl.exe: fitting_fgsl.o
	$(FC) $(FFLAGS) -o $@ $(IDIR) $^ $(LIBS)

fitting_fgsl.o: fitting_fgsl.f90
	$(FC) -c $(FFLAGS) -o $@ $(IDIR) $<

debug: fitting_fgsl.f90
	$(FC) -g $(FDEBUGFLAGS) -o fittingfgsl_dbg.exe $(IDIR) $^ $(LIBS)

clean:
	rm -f *.o *.mod *.exe