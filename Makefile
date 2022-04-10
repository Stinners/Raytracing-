compile: 
	gfortran -c ./src/vec3.f90
	gfortran -c ./src/ray.f90
	gfortran -c ./src/main.f90 
	gfortran *.o -fdefault-real-8 -o dist/main

run:
	dist/main

clean:
	rm -f *.o
	rm -f *.mod

test: compile run
