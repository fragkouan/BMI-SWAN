cmake_minimum_required(VERSION 3.0)

project(bmi-swan Fortran)

set(bmiswan_lib bmiswan)

# Locate the installed BMI bindings
find_library(bmif_lib bmif
	PATHS /home/s2006658/myenv/local/lib)
find_path(bmif_inc bmif_2_0.mod
	PATHS /home/s2006658/myenv/local/include)
include_directories(${bmif_inc})

# Locate the SWAN library
find_library(swan_lib swanmodel
PATHS /home/s2006658/Dropbox/bitbucket_repo/bmi_swan_atropos/swan)
set(EXTRA_LIBS
  ${bmif_lib}
  ${swan_lib}
)

message(STATUS "bmif: ${bmif_lib}")
message(STATUS "SWAN: ${swan_lib}")

add_subdirectory(src)
