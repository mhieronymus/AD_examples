FC=gfortran-6
GFFLAGS=-g

all: valder  tests main exe

valder: valder.f08 
	$(FC) $(GFFLAGS) -c valder.f08 

tests: tests.f08 
	$(FC) $(GFFLAGS) -c tests.f08 

main: main.f08 
	$(FC) $(GFFLAGS) -c main.f08 

exe:
	$(FC) -o test *.o

clean:
	rm -f *.mod