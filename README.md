# BMI-SWAN

The Basic Model Interface (BMI; Hutton et al. 2020)for the spectral wave model Simulating WAves Nearshore (SWAN; Booij et al. 1999) is provided here. 
The original SWAN code, as well as its official documentation can be found at: https://swanmodel.sourceforge.io/
The Basic Model Interface and its documentation is available here: https://bmi.readthedocs.io/en/stable/

Here we provide the necessary components to be able to configure BMI-SWAN in a Linux machine:
1) BMI-The Fortran Specification: It is available through https://github.com/csdms/bmi-fortran
2) CMake minimum version 3.0 (https://cmake.org/)

In case you want to couple BMI with the hydrodynamics coastal ocean model Thetis, we recommend performing the configuration inside the "firedrake" environment.

# Installation instructions
1) Install BMI-Fortran
  a. Download the bmi-fortran code (https://github.com/csdms/bmi-fortran) and copy it inside this repository
