compile: 
	gfortran -c ./src/random.f90
	gfortran -c ./src/vec3.f90
	gfortran -c ./src/ray.f90
	gfortran -c ./src/hittable.f90
	gfortran -c ./src/sphere.f90
	gfortran -c ./src/world.f90
	gfortran -c ./src/camera.f90
	gfortran -c ./src/main.f90 
	gfortran *.o -O2 -fdefault-real-8 -o dist/main

run:
	dist/main

clean:
	rm -f *.o
	rm -f *.mod

test_build: compile clean 

test: compile run
