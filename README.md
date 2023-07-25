# BMI-SWAN

The Basic Model Interface (BMI; Hutton et al. 2020) for the spectral wave model Simulating WAves Nearshore (SWAN; Booij et al. 1999) is provided here. 
The original SWAN code, as well as its official documentation can be found at: https://swanmodel.sourceforge.io/ <br>
The Basic Model Interface and its documentation is available here: https://bmi.readthedocs.io/en/stable/

Here we provide the necessary components to be able to configure BMI-SWAN in a Linux machine:
1) BMI-The Fortran Specification: It is available through https://github.com/csdms/bmi-fortran
2) CMake minimum version 3.0 (https://cmake.org/)

In case you want to couple BMI with the hydrodynamics coastal ocean model Thetis, we recommend performing the configuration inside the "firedrake" environment.

# Installation instructions
The instructions provided here are for an installation directory that is not the /usr/local, but a directory called 'myenv" i.e. <path>/myenv/local

**A. Install BMI-Fortran** 

 a. Download the bmi-fortran code (https://github.com/csdms/bmi-fortran) and copy it inside this repository. Its name should be bmi-fortran <br> 
 b. cd ./bmi-fortran && mkdir _build && cd _build <br> 
 c. cmake .. -DCMAKE_INSTALL_PREFIX=<path>/myenv/local <br>
 d. make && make install <br>
 e. cd ../../ <br>

**B. Create SWAN's shared library**

 a. cd ./swan <br>
 b. make files <br>
 c. make compile <br>
 d. cd ../ <br>

**C. Create BMI-SWAN library and its Python package"
 
 a. cd ./bmi-swan && mkdir _build && cd _build <br>
 b. Change the paths in the CMakeLists (3 paths should be changed, the one in find_library <path>/myenv/local/lib, the other in find_path <path>/myenv/local/include, and the SWAN find library) <br>
 c. Change the paths in ./src/Makefile and ./src/setup.py <br>
 d. cmake .. -DCMAKE_INSTALL_PREFIX=<path>/myenv/local <br>
 e. make <br>
 f. make install <br>
 g. cd ../src <br>
 h. make <br>
 i. python3 setup.py build_ext --inplace <br>

**D. Include the necessary Path**

 a. Include the <path>/myenv/local/lib and the <path1>/BMI-SWAN/swan to the LD_LIBRARY_PATH <br>
 b. Add the <path1>/BMI-SWAN/bmi-swan/src to the PYTHONPATH <br>
 
 In case of the installation of BMI-SWAN within the firedrake environment, see activate file and implement necessary changes into your own activate file located in the firedrake/source/bin directory, so as to add those paths only when firedrake environemnt is activated
 
 
    
