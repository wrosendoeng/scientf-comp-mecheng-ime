FC = gfortran
FFLAGS = -march=native -mtune=native -O3
FDEBUGFLAGS = -g -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal
IDIR = -I/usr/local/include/fgsl/
LIBS = -lgsl -lfgsl -lopenblas -lm

objects: approximation.exe cubicspline.exe

all: $(objects)

approximation.exe: approximation.f90 direct_resolution_methods.o mmq.o
	$(FC) $(FFLAGS) -o $@ $(IDIR) $^ $(LIBS)

cubicspline.exe: interpolar.f90 direct_resolution_methods.o cubicspline.o
	$(FC) $(FFLAGS) -o $@ $(IDIR) $^ $(LIBS)

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $@ $(IDIR) $<

debug: approximation.f90 direct_resolution_methods.f90 mmq.f90
	$(FC) -g $(FDEBUGFLAGS) -o dbg_approximation.exe $(IDIR) $^ $(LIBS)

debug2: interpolar.f90 direct_resolution_methods.f90 cubicspline.f90
	$(FC) -g $(FDEBUGFLAGS) -o dbg_cubicspline.exe $(IDIR) $^ $(LIBS)

clean:
	rm -f *.o *.mod *.exe
