FC = gfortran
FFLAGS = -march=native -mtune=native -O3
FDEBUGFLAGS = -g -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal
IDIR = -I/usr/local/include/fgsl/
LIBS = -lgsl -lfgsl -lopenblas -lm

all: approximation.exe

approximation.exe: approximation.f90 direct_resolution_methods.o mmq.o
	$(FC) $(FFLAGS) -o $@ $(IDIR) $^ $(LIBS)

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $@ $(IDIR) $<

debug: approximation.f90 direct_resolution_methods.f90 mmq.f90
	$(FC) -g $(FDEBUGFLAGS) -o approximation_dbg.exe $(IDIR) $^ $(LIBS)

clean:
	rm -f *.o *.mod *.exe
