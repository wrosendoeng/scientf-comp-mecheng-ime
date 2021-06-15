FC = gfortran
FFLAGS = -march=native -mtune=native -O3 #-fopenmp 
FDEBUGFLAGS = -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal
#IDIR = -I/usr/local/include/fgsl/
#LIBS = -lgsl -lfgsl -lopenblas -lm
LIBS = -lopenblas -lm

all: heat2dequation.exe

heat2dequation.exe: main.o parameters.o
	$(FC) $(FFLAGS) -o $@ $(IDIR) $^ $(LIBS)

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $@ $(IDIR) $<

debug: main.f90 parameters.f90
	$(FC) -g $(FDEBUGFLAGS) -o dbgheat2.exe $(IDIR) $^ $(LIBS)

clean:
	rm -f *.o *.mod *.exe